;;; ob-tmux-capture.el --- Capture tmux pane output into org results -*- lexical-binding: t; -*-

;; Author: ii research
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: org, tmux, literate programming

;;; Commentary:

;; This package adds output capture to ob-tmux/ob-tmate blocks.
;;
;; The problem: ob-tmux sends commands to tmux but doesn't capture output.
;; This package wraps commands with unique markers, polls for completion,
;; and extracts the output for #+RESULTS: blocks.
;;
;; Capture methods:
;; - `markers' (default) - bracket command with BEGIN/END markers
;; - `asciinema' (planned) - wrap in asciinema recording
;;
;; Usage:
;;   #+begin_src tmux :session main :capture markers
;;   kubectl get pods
;;   #+end_src
;;
;; Or interactively after execution:
;;   M-x ob-tmux-capture-last-output

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)

;; Optional: load asciinema configuration if available
(require 'ob-tmux-asciinema nil t)

;;; Customization

(defgroup ob-tmux-capture nil
  "Capture output from tmux/tmate org-babel blocks."
  :group 'org-babel
  :prefix "ob-tmux-capture-")

(defcustom ob-tmux-capture-default-method 'markers
  "Default capture method.
- `markers': Bracket command with unique BEGIN/END markers
- `asciinema': Record with asciinema (not yet implemented)"
  :type '(choice (const :tag "Bracket markers" markers)
                 (const :tag "Asciinema recording" asciinema))
  :group 'ob-tmux-capture)

(defcustom ob-tmux-capture-timeout 30
  "Default timeout in seconds waiting for command completion."
  :type 'integer
  :group 'ob-tmux-capture)

(defcustom ob-tmux-capture-poll-interval 0.2
  "Interval in seconds between polling for completion."
  :type 'number
  :group 'ob-tmux-capture)

(defcustom ob-tmux-capture-auto-upload t
  "Whether to automatically upload cast files to server after completion.
When non-nil, cast files are uploaded and a recording URL is added to history."
  :type 'boolean
  :group 'ob-tmux-capture)

(defcustom ob-tmux-capture-cast-history t
  "Whether to maintain cast history in org file.
When non-nil, recording URLs and timestamps are appended to #+CAST_HISTORY table."
  :type 'boolean
  :group 'ob-tmux-capture)

(defcustom ob-tmux-capture-marker-prefix "___OB_TMUX_"
  "Prefix for capture markers."
  :type 'string
  :group 'ob-tmux-capture)

(defcustom ob-tmux-capture-strip-ansi t
  "Whether to strip ANSI escape sequences from captured output."
  :type 'boolean
  :group 'ob-tmux-capture)

(defcustom ob-tmux-capture-strip-prompt t
  "Whether to attempt stripping shell prompts from output."
  :type 'boolean
  :group 'ob-tmux-capture)

(defcustom ob-tmux-capture-cast-base-dir
  (expand-file-name "ob-tmux-casts"
                    (or (getenv "XDG_DATA_HOME")
                        (expand-file-name "~/.local/share")))
  "Base directory for storing cast files.
Default is $XDG_DATA_HOME/ob-tmux-casts or ~/.local/share/ob-tmux-casts.
Can be overridden per-block with `:cast-dir' header arg.
Use `:cast-dir local' to store alongside the org file."
  :type 'directory
  :group 'ob-tmux-capture)

(defcustom ob-tmux-capture-cast-local-subdir ".ob-tmux-casts"
  "Subdirectory name when using `:cast-dir local'.
Created next to the org file."
  :type 'string
  :group 'ob-tmux-capture)

;;; Cast File Path Generation

(defun ob-tmux-capture--slugify (string)
  "Convert STRING to a safe filename slug."
  (let ((slug (downcase string)))
    ;; Replace unsafe chars with dashes
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    ;; Remove leading/trailing dashes
    (setq slug (replace-regexp-in-string "^-+\\|-+$" "" slug))
    ;; Truncate to reasonable length
    (if (> (length slug) 50)
        (substring slug 0 50)
      slug)))

(defun ob-tmux-capture--get-context-for-path (params)
  "Extract context from PARAMS and buffer for building cast path.
Returns plist with :org-file :org-dir :session :window :heading :block-name."
  (let* ((session-full (or (cdr (assq :session params)) "default"))
         ;; Parse session:window format
         (session-parts (split-string session-full ":"))
         (session (car session-parts))
         (window (or (cadr session-parts) "main"))
         ;; Org file info
         (org-file (buffer-file-name))
         (org-dir (when org-file (file-name-directory org-file)))
         (org-basename (when org-file
                         (file-name-sans-extension
                          (file-name-nondirectory org-file))))
         ;; Block context
         (block-name (org-element-property :name (org-element-at-point)))
         (heading (save-excursion
                    (when (org-up-heading-safe)
                      (org-get-heading t t t t)))))
    (list :org-file org-file
          :org-dir org-dir
          :org-basename org-basename
          :session session
          :window window
          :heading heading
          :block-name block-name)))

(defun ob-tmux-capture--build-cast-path (params)
  "Build full path for cast file based on PARAMS and context.
Respects :cast-dir header arg:
  - `local' or \"local\": store next to org file
  - absolute path: use as base directory
  - nil: use `ob-tmux-capture-cast-base-dir' (XDG default)"
  (let* ((cast-dir-arg (cdr (assq :cast-dir params)))
         (ctx (ob-tmux-capture--get-context-for-path params))
         (org-file (plist-get ctx :org-file))
         (org-dir (plist-get ctx :org-dir))
         (org-basename (or (plist-get ctx :org-basename) "unknown"))
         (session (plist-get ctx :session))
         (window (plist-get ctx :window))
         (heading (plist-get ctx :heading))
         (block-name (plist-get ctx :block-name))
         ;; Build base directory
         (base-dir
          (cond
           ;; :cast-dir local - store next to org file
           ((member cast-dir-arg '(local "local"))
            (if org-dir
                (expand-file-name ob-tmux-capture-cast-local-subdir org-dir)
              (expand-file-name ob-tmux-capture-cast-local-subdir)))
           ;; :cast-dir /some/path - use explicit path
           ((and cast-dir-arg (file-name-absolute-p cast-dir-arg))
            cast-dir-arg)
           ;; Default: XDG base dir
           (t ob-tmux-capture-cast-base-dir)))
         ;; Build subdirectory: org-basename/session_window
         (sub-dir (expand-file-name
                   (format "%s/%s_%s" org-basename session window)
                   base-dir))
         ;; Build filename: timestamp_context.cast
         (timestamp (format-time-string "%Y-%m-%dT%H%M%S"))
         (context-slug (ob-tmux-capture--slugify
                        (or block-name heading "block")))
         (filename (format "%s_%s.cast" timestamp context-slug)))
    ;; Ensure directory exists
    (unless (file-directory-p sub-dir)
      (make-directory sub-dir t))
    ;; Return full path
    (expand-file-name filename sub-dir)))

;;; Marker Generation

(defun ob-tmux-capture--generate-id ()
  "Generate a unique ID for markers."
  (let ((random-bytes (make-string 8 0)))
    (dotimes (i 8)
      (aset random-bytes i (+ 33 (random 94))))  ; printable ASCII
    (secure-hash 'md5 (format "%s%s%s"
                               random-bytes
                               (current-time)
                               (emacs-pid)))))

(defun ob-tmux-capture--make-markers ()
  "Generate a unique BEGIN/END marker pair.
Returns (BEGIN-MARKER . END-MARKER)."
  (let ((id (substring (ob-tmux-capture--generate-id) 0 12)))
    (cons (format "%sBEGIN_%s___" ob-tmux-capture-marker-prefix id)
          (format "%sEND_%s___" ob-tmux-capture-marker-prefix id))))

;;; tmux Interaction

(defun ob-tmux-capture--build-tmux-cmd (socket &rest args)
  "Build tmux command with optional SOCKET and ARGS."
  (let ((cmd (list "tmux")))
    (when (and socket (not (string-empty-p socket)))
      (setq cmd (append cmd (list "-S" socket))))
    (append cmd args)))

(defun ob-tmux-capture--send-keys (session socket keys)
  "Send KEYS to tmux SESSION (optionally via SOCKET)."
  (let* ((cmd (ob-tmux-capture--build-tmux-cmd
               socket "send-keys" "-t" session keys "Enter"))
         (cmd-string (mapconcat #'shell-quote-argument cmd " ")))
    (shell-command-to-string cmd-string)))

(defun ob-tmux-capture--capture-pane (session socket &optional lines)
  "Capture content from tmux SESSION pane (optionally via SOCKET).
LINES specifies how many lines of history to capture.
If nil, captures just the visible pane."
  (let* ((cmd (if lines
                  (ob-tmux-capture--build-tmux-cmd
                   socket "capture-pane" "-p" "-S" (format "-%d" lines) "-t" session)
                (ob-tmux-capture--build-tmux-cmd
                 socket "capture-pane" "-p" "-t" session)))
         (cmd-string (mapconcat #'shell-quote-argument cmd " ")))
    (shell-command-to-string cmd-string)))

;;; Pane Environment Variables
;;
;; Functions for storing pane → stream mappings in tmux environment.
;; This allows runtime tracking of which stream is recording each pane.

(defun ob-tmux-capture--get-pane-env (session socket var)
  "Get environment variable VAR from tmux SESSION (optionally via SOCKET)."
  (let* ((cmd (ob-tmux-capture--build-tmux-cmd
               socket "show-environment" "-t" session var))
         (cmd-string (mapconcat #'shell-quote-argument cmd " "))
         (result (string-trim (shell-command-to-string cmd-string))))
    ;; Result is "VAR=value" or "unknown variable: VAR"
    (when (and result (string-match (format "^%s=\\(.*\\)$" (regexp-quote var)) result))
      (match-string 1 result))))

(defun ob-tmux-capture--set-pane-env (session socket var value)
  "Set environment variable VAR to VALUE in tmux SESSION (optionally via SOCKET)."
  (let* ((cmd (ob-tmux-capture--build-tmux-cmd
               socket "set-environment" "-t" session var value))
         (cmd-string (mapconcat #'shell-quote-argument cmd " ")))
    (shell-command-to-string cmd-string)
    value))

(defun ob-tmux-capture--unset-pane-env (session socket var)
  "Unset environment variable VAR from tmux SESSION (optionally via SOCKET)."
  (let* ((cmd (ob-tmux-capture--build-tmux-cmd
               socket "set-environment" "-t" session "-u" var))
         (cmd-string (mapconcat #'shell-quote-argument cmd " ")))
    (shell-command-to-string cmd-string)))

(defun ob-tmux-capture-get-pane-stream (session socket)
  "Get stream info for tmux SESSION pane (optionally via SOCKET).
Returns alist with :id, :url, :token keys if stream is configured, nil otherwise."
  (let ((stream-id (ob-tmux-capture--get-pane-env session socket "OB_TMUX_STREAM_ID"))
        (stream-url (ob-tmux-capture--get-pane-env session socket "OB_TMUX_STREAM_URL"))
        (stream-token (ob-tmux-capture--get-pane-env session socket "OB_TMUX_STREAM_TOKEN")))
    (when stream-id
      (list (cons :id stream-id)
            (cons :url stream-url)
            (cons :token stream-token)))))

(defun ob-tmux-capture-set-pane-stream (session socket stream-id &optional stream-url stream-token)
  "Set stream info for tmux SESSION pane (optionally via SOCKET).
STREAM-ID is required, STREAM-URL and STREAM-TOKEN are optional."
  (ob-tmux-capture--set-pane-env session socket "OB_TMUX_STREAM_ID" stream-id)
  (when stream-url
    (ob-tmux-capture--set-pane-env session socket "OB_TMUX_STREAM_URL" stream-url))
  (when stream-token
    (ob-tmux-capture--set-pane-env session socket "OB_TMUX_STREAM_TOKEN" stream-token))
  (list (cons :id stream-id)
        (cons :url stream-url)
        (cons :token stream-token)))

(defun ob-tmux-capture-clear-pane-stream (session socket)
  "Clear stream info from tmux SESSION pane (optionally via SOCKET)."
  (ob-tmux-capture--unset-pane-env session socket "OB_TMUX_STREAM_ID")
  (ob-tmux-capture--unset-pane-env session socket "OB_TMUX_STREAM_URL")
  (ob-tmux-capture--unset-pane-env session socket "OB_TMUX_STREAM_TOKEN"))

(defun ob-tmux-capture--derive-stream-name (session)
  "Derive a stream name from SESSION identifier.
SESSION is typically \"session:window\" format.
Returns sanitized string suitable for stream naming."
  (let ((name (replace-regexp-in-string ":" "_" session)))
    (replace-regexp-in-string "[^a-zA-Z0-9_-]" "" name)))

;;; Pane Stream Setup
;;
;; Functions for creating panes with asciinema session wrapper.
;; This enables long-running recording of pane activity.

(defcustom ob-tmux-capture-pane-stream-enabled nil
  "Whether to automatically wrap panes with asciinema session.
When non-nil, new panes are created with asciinema session wrapper
for continuous recording.  This can be overridden per-block with
`:pane-stream' header arg."
  :type 'boolean
  :group 'ob-tmux-capture)

(defcustom ob-tmux-capture-asciinema-path "asciinema"
  "Path to asciinema CLI executable."
  :type 'string
  :group 'ob-tmux-capture)

(defcustom ob-tmux-capture-default-shell (or (getenv "SHELL") "bash")
  "Default shell to run inside asciinema session."
  :type 'string
  :group 'ob-tmux-capture)

(defun ob-tmux-capture--pane-exists-p (session socket)
  "Check if tmux pane for SESSION exists (optionally via SOCKET)."
  (let* ((cmd (ob-tmux-capture--build-tmux-cmd
               socket "has-session" "-t" session))
         (cmd-string (mapconcat #'shell-quote-argument cmd " ")))
    (zerop (call-process-shell-command cmd-string nil nil nil))))

(defun ob-tmux-capture--pane-is-wrapped-p (session socket)
  "Check if tmux pane for SESSION is already wrapped with asciinema.
Returns non-nil if the ASCIINEMA_SESSION env var is set in the pane."
  ;; Check if asciinema session is running by looking for the env var it sets
  (let ((asciinema-session (ob-tmux-capture--get-pane-env
                            session socket "ASCIINEMA_SESSION")))
    (and asciinema-session (not (string-empty-p asciinema-session)))))

(defun ob-tmux-capture--build-asciinema-session-cmd (&optional stream-id cast-file local-port title)
  "Build asciinema session command.
At least one of STREAM-ID, CAST-FILE, or LOCAL-PORT must be provided.
STREAM-ID: Stream to remote server. Use t or \"auto\" to auto-create.
CAST-FILE: Record to local file.
LOCAL-PORT: Start local streaming server (e.g., \"0.0.0.0:8765\").
TITLE: Optional title for the stream."
  (let ((cmd (list ob-tmux-capture-asciinema-path "session")))
    (when stream-id
      (if (or (eq stream-id t) (equal stream-id "auto"))
          ;; Auto-create: --stream-remote without value
          (setq cmd (append cmd (list "--stream-remote")))
        ;; Specific ID: --stream-remote ID
        (setq cmd (append cmd (list "--stream-remote" stream-id)))))
    (when cast-file
      (setq cmd (append cmd (list "--output-file" cast-file))))
    (when local-port
      (setq cmd (append cmd (list "--stream-local" local-port))))
    (when title
      (setq cmd (append cmd (list "--title" title))))
    ;; Must have at least one output mode
    (unless (or stream-id cast-file local-port)
      (error "asciinema session requires at least one of: stream-id, cast-file, or local-port"))
    (setq cmd (append cmd (list ob-tmux-capture-default-shell)))
    (mapconcat #'shell-quote-argument cmd " ")))

(defun ob-tmux-capture-setup-pane-stream (session socket stream-info)
  "Set up SESSION pane (via SOCKET) with asciinema session recording.
STREAM-INFO is an alist that can contain:
  :id - Stream ID for remote streaming (use t or \"auto\" to auto-create)
  :url - Stream URL
  :token - Stream token
  :cast-file - Local cast file path
  :local-port - Local streaming port (e.g., \"0.0.0.0:8765\")
  :title - Title for the stream

At least one of :id, :cast-file, or :local-port must be provided.
Use `:id t` or `:id \"auto\"` to auto-create a new stream on the server.
Returns the stream info if successful, nil otherwise.

This creates a new tmux window wrapped with asciinema session.
If the pane already exists and is wrapped, returns existing stream info."
  ;; Check if already wrapped
  (if (and (ob-tmux-capture--pane-exists-p session socket)
           (ob-tmux-capture--pane-is-wrapped-p session socket))
      ;; Already wrapped - return existing info
      (let ((existing (ob-tmux-capture-get-pane-stream session socket)))
        (when existing
          (message "Pane %s already has stream: %s" session (alist-get :id existing)))
        existing)
    ;; Need to set up new pane with stream
    (let* ((stream-id (alist-get :id stream-info))
           (stream-url (alist-get :url stream-info))
           (stream-token (alist-get :token stream-info))
           (cast-file (alist-get :cast-file stream-info))
           (local-port (alist-get :local-port stream-info))
           (title (alist-get :title stream-info))
           ;; Parse session:window format
           (parts (split-string session ":"))
           (tmux-session (car parts))
           (tmux-window (or (cadr parts) "main"))
           ;; Build the asciinema wrapper command
           (wrapper-cmd (ob-tmux-capture--build-asciinema-session-cmd
                         stream-id cast-file local-port title)))

      ;; Check if session exists
      (let ((ensure-cmd (ob-tmux-capture--build-tmux-cmd
                         socket "has-session" "-t" tmux-session)))
        (if (zerop (call-process-shell-command
                    (mapconcat #'shell-quote-argument ensure-cmd " ")
                    nil nil nil))
            ;; Session exists, create new window
            ;; Note: wrapper-cmd needs to be single-quoted for tmux shell interpretation
            (let* ((base-cmd (ob-tmux-capture--build-tmux-cmd
                              socket "new-window" "-t"
                              (format "%s:%s" tmux-session tmux-window)))
                   ;; Single-quote the wrapper command for shell
                   (quoted-wrapper (format "'%s'" (replace-regexp-in-string
                                                   "'" "'\\''" wrapper-cmd)))
                   (cmd-string (concat
                                (mapconcat #'shell-quote-argument base-cmd " ")
                                " " quoted-wrapper)))
              (shell-command-to-string cmd-string))
          ;; Create session with first window
          (let* ((base-cmd (ob-tmux-capture--build-tmux-cmd
                            socket "new-session" "-d" "-s" tmux-session
                            "-n" tmux-window))
                 ;; Single-quote the wrapper command for shell
                 (quoted-wrapper (format "'%s'" (replace-regexp-in-string
                                                 "'" "'\\''" wrapper-cmd)))
                 (cmd-string (concat
                              (mapconcat #'shell-quote-argument base-cmd " ")
                              " " quoted-wrapper)))
            (shell-command-to-string cmd-string))))

      ;; Give it a moment to start
      (sleep-for 0.5)
      ;; Store stream info in the session's environment
      ;; For auto-created streams (t), use "auto" as placeholder until we can capture real ID
      (ob-tmux-capture-set-pane-stream session socket
                                       (cond ((eq stream-id t) "auto")
                                             ((stringp stream-id) stream-id)
                                             (t "local"))
                                       stream-url
                                       stream-token)
      stream-info)))

;;; Output Cleaning

(defun ob-tmux-capture--strip-ansi (string)
  "Remove ANSI escape sequences from STRING."
  (replace-regexp-in-string "\033\\[[0-9;]*[a-zA-Z]" "" string))

(defun ob-tmux-capture--strip-commands (output body)
  "Strip command echoes and prompt-only lines from OUTPUT.
BODY is the original commands we sent - lines matching these are removed.
Also removes prompt-only lines and typical CWD lines that appear with prompts."
  (when output
    (let* ((body-lines (split-string body "\n" t))
           ;; Common prompt patterns to strip (prompt chars alone)
           (prompt-re "^[[:space:]]*\\(?:[$#%>❯→▶]\\|\\[.*\\][$#]\\)[[:space:]]*$")
           ;; CWD lines (just a path, often shown above prompt)
           (cwd-re "^[[:space:]]*/[a-zA-Z0-9/_.-]*[[:space:]]*$")
           ;; Build patterns from body lines
           (cmd-patterns (mapcar (lambda (line)
                                   (regexp-quote (string-trim line)))
                                 body-lines))
           (output-lines (split-string output "\n")))
      ;; Filter out prompt-only lines and lines containing our commands
      (setq output-lines
            (seq-filter
             (lambda (line)
               (let ((trimmed (string-trim line)))
                 (and
                  ;; Not empty
                  (not (string-empty-p trimmed))
                  ;; Not just a prompt char
                  (not (string-match-p prompt-re line))
                  ;; Not just a CWD path
                  (not (string-match-p cwd-re line))
                  ;; Not a line containing one of our commands
                  (not (seq-some (lambda (pat)
                                   (string-match-p pat trimmed))
                                 cmd-patterns)))))
             output-lines))
      (string-join output-lines "\n"))))

(defun ob-tmux-capture--extract-between-markers (output begin-marker end-marker)
  "Extract content between BEGIN-MARKER and END-MARKER in OUTPUT.
Returns nil if markers not found.

The output looks like:
  $ echo 'BEGIN_MARKER'
  BEGIN_MARKER
  $ actual command
  actual output
  $ echo 'END_MARKER'
  END_MARKER

We want everything after the BEGIN_MARKER output line (not the echo command)
and before the echo command for END_MARKER."
  ;; Match marker at start of line (the output, not inside echo command)
  (let ((begin-re (concat "^" (regexp-quote begin-marker)))
        (end-re (concat "echo ['\"]?" (regexp-quote end-marker))))
    (when (string-match begin-re output)
      ;; Find end of line containing BEGIN marker
      (let ((after-begin-line (when (string-match "\n" output (match-end 0))
                                (match-end 0))))
        (when (and after-begin-line
                   (string-match end-re output after-begin-line))
          ;; end-match-start is where "echo 'END..." starts
          (let ((end-line-start (match-beginning 0)))
            ;; Back up to start of this line (skip the prompt)
            (setq end-line-start
                  (let ((pos end-line-start))
                    (while (and (> pos 0)
                                (not (eq (aref output (1- pos)) ?\n)))
                      (setq pos (1- pos)))
                    pos))
            (when (< after-begin-line end-line-start)
              (substring output after-begin-line end-line-start))))))))

(defun ob-tmux-capture--clean-output (output)
  "Clean OUTPUT by stripping ANSI codes and normalizing whitespace."
  (let ((cleaned output))
    (when ob-tmux-capture-strip-ansi
      (setq cleaned (ob-tmux-capture--strip-ansi cleaned)))
    ;; Normalize line endings
    (setq cleaned (replace-regexp-in-string "\r\n" "\n" cleaned))
    (setq cleaned (replace-regexp-in-string "\r" "" cleaned))
    ;; Trim trailing whitespace from each line
    (setq cleaned (replace-regexp-in-string "[ \t]+$" "" cleaned))
    ;; Trim leading/trailing blank lines
    (string-trim cleaned)))

;;; Marker-Based Capture

(defun ob-tmux-capture--wrap-with-markers (body markers)
  "Wrap BODY with echo statements for MARKERS (BEGIN . END)."
  (let ((begin-marker (car markers))
        (end-marker (cdr markers)))
    (format "echo '%s'\n%s\necho '%s'" begin-marker body end-marker)))

(defun ob-tmux-capture--wait-for-marker (session socket marker timeout)
  "Poll until MARKER appears in SESSION pane or TIMEOUT seconds elapse.
Returns captured pane content if marker found, nil on timeout."
  (let ((start-time (current-time))
        (found nil)
        (output nil))
    (while (and (not found)
                (< (float-time (time-subtract (current-time) start-time))
                   timeout))
      (setq output (ob-tmux-capture--capture-pane session socket))
      (when (string-match-p (regexp-quote marker) output)
        (setq found t))
      (unless found
        (sleep-for ob-tmux-capture-poll-interval)))
    (when found output)))

(defun ob-tmux-capture-execute-with-markers (body session socket &optional timeout raw)
  "Execute BODY in tmux SESSION with marker-based capture.
SOCKET is optional tmux socket path.
TIMEOUT in seconds (default `ob-tmux-capture-timeout').
RAW if non-nil, include prompts and command echoes in output.
Returns captured output string or nil on timeout."
  (let* ((timeout (or timeout ob-tmux-capture-timeout))
         (markers (ob-tmux-capture--make-markers))
         (begin-marker (car markers))
         (end-marker (cdr markers))
         (wrapped-body (ob-tmux-capture--wrap-with-markers body markers)))
    ;; Send wrapped command
    (ob-tmux-capture--send-keys session socket wrapped-body)
    ;; Wait for end marker
    (let ((raw-output (ob-tmux-capture--wait-for-marker
                       session socket end-marker timeout)))
      (if raw-output
          ;; Extract and clean
          (let* ((extracted (ob-tmux-capture--extract-between-markers
                             raw-output begin-marker end-marker))
                 (cleaned (when extracted
                            (ob-tmux-capture--clean-output extracted))))
            ;; Strip commands unless :raw
            (if (and cleaned (not raw))
                (ob-tmux-capture--strip-commands cleaned body)
              cleaned))
        (message "ob-tmux-capture: Timeout waiting for command completion")
        nil))))

;;; Streaming Capture

(defvar ob-tmux-capture-streams (make-hash-table :test 'equal)
  "Active streaming captures. Key: stream-id, Value: plist of state.")

(defvar ob-tmux-capture-stream-interval 0.3
  "Polling interval in seconds for streaming capture.")

(defun ob-tmux-capture-stream--make-id ()
  "Generate unique stream ID."
  (format "stream-%s-%s" (float-time) (random 10000)))

(defun ob-tmux-capture-stream-start (body session socket &optional timeout)
  "Start streaming capture of BODY execution in SESSION.
Returns stream ID. Output streams to #+RESULTS: block in real-time."
  (let* ((id (ob-tmux-capture-stream--make-id))
         (timeout (or timeout ob-tmux-capture-timeout))
         (markers (ob-tmux-capture--make-markers))
         (begin-marker (car markers))
         (end-marker (cdr markers))
         (wrapped-body (ob-tmux-capture--wrap-with-markers body markers))
         ;; Remember where to insert results
         (result-marker (point-marker))
         (state (list :id id
                      :session session
                      :socket socket
                      :begin-marker begin-marker
                      :end-marker end-marker
                      :result-marker result-marker
                      :buffer (current-buffer)
                      :start-time (float-time)
                      :timeout timeout
                      :last-capture ""
                      :started nil    ; have we seen BEGIN marker?
                      :finished nil
                      :timer nil)))
    ;; Store state
    (puthash id state ob-tmux-capture-streams)
    ;; Insert initial empty results block
    (save-excursion
      (goto-char result-marker)
      (org-babel-insert-result "" '("output" "replace")))
    ;; Send command
    (ob-tmux-capture--send-keys session socket wrapped-body)
    ;; Start polling timer
    (let ((timer (run-with-timer
                  ob-tmux-capture-stream-interval
                  ob-tmux-capture-stream-interval
                  #'ob-tmux-capture-stream--tick id)))
      (plist-put state :timer timer))
    (message "Streaming capture started: %s" id)
    id))

(defun ob-tmux-capture-stream--tick (id)
  "Poll and update stream ID with new output."
  (let ((state (gethash id ob-tmux-capture-streams)))
    (when (and state (not (plist-get state :finished)))
      (let* ((session (plist-get state :session))
             (socket (plist-get state :socket))
             (begin-marker (plist-get state :begin-marker))
             (end-marker (plist-get state :end-marker))
             (result-marker (plist-get state :result-marker))
             (buf (plist-get state :buffer))
             (start-time (plist-get state :start-time))
             (timeout (plist-get state :timeout))
             (last-capture (plist-get state :last-capture))
             (started (plist-get state :started))
             ;; Capture current pane
             (current-capture (ob-tmux-capture--capture-pane session socket)))
        ;; Check timeout
        (when (> (- (float-time) start-time) timeout)
          (ob-tmux-capture-stream-stop id "Timeout")
          (cl-return-from ob-tmux-capture-stream--tick))
        ;; Check if we've started (seen BEGIN marker)
        (unless started
          (when (string-match-p (regexp-quote begin-marker) current-capture)
            (plist-put state :started t)
            (setq started t)))
        ;; If started, extract and update content
        (when started
          (let ((content (ob-tmux-capture-stream--extract-content
                          current-capture begin-marker end-marker)))
            ;; Update results block if content changed
            (when (and content (not (string= content last-capture)))
              (plist-put state :last-capture content)
              (ob-tmux-capture-stream--update-result buf result-marker content))
            ;; Check if finished (END marker seen)
            (when (string-match-p (concat "echo ['\"]?" (regexp-quote end-marker))
                                  current-capture)
              (ob-tmux-capture-stream-stop id "Complete"))))))))

(defun ob-tmux-capture-stream--extract-content (capture begin-marker end-marker)
  "Extract content from CAPTURE between markers (partial OK)."
  ;; Find BEGIN marker at start of line
  (let ((begin-re (concat "^" (regexp-quote begin-marker))))
    (when (string-match begin-re capture)
      (let ((after-begin (when (string-match "\n" capture (match-end 0))
                           (match-end 0))))
        (when after-begin
          ;; Look for END marker echo command
          (let ((end-re (concat "echo ['\"]?" (regexp-quote end-marker))))
            (if (string-match end-re capture after-begin)
                ;; END found - extract up to start of that line
                (let ((end-line-start (match-beginning 0)))
                  (setq end-line-start
                        (let ((pos end-line-start))
                          (while (and (> pos 0)
                                      (not (eq (aref capture (1- pos)) ?\n)))
                            (setq pos (1- pos)))
                          pos))
                  (when (< after-begin end-line-start)
                    (ob-tmux-capture--clean-output
                     (substring capture after-begin end-line-start))))
              ;; END not yet - return everything after BEGIN
              (ob-tmux-capture--clean-output
               (substring capture after-begin)))))))))

(defun ob-tmux-capture-stream--update-result (buffer marker content)
  "Update results block in BUFFER at MARKER with CONTENT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (when (re-search-forward "#\\+RESULTS:" nil t)
          (forward-line 1)
          (let ((results-start (point))
                (results-end (point)))
            ;; Find end of results block
            (while (and (not (eobp))
                        (looking-at-p "^\\(: \\|#\\+\\|[[:space:]]*$\\)"))
              (forward-line 1))
            (setq results-end (point))
            ;; Delete and replace
            (delete-region results-start results-end)
            (goto-char results-start)
            (dolist (line (split-string content "\n"))
              (insert ": " line "\n"))))))))

(defun ob-tmux-capture-stream-stop (id &optional reason)
  "Stop streaming capture ID."
  (let ((state (gethash id ob-tmux-capture-streams)))
    (when state
      (let ((timer (plist-get state :timer)))
        (when timer
          (cancel-timer timer)))
      (plist-put state :finished t)
      (message "Streaming capture %s: %s" id (or reason "Stopped"))
      ;; Clean up after a delay
      (run-with-timer 5 nil #'remhash id ob-tmux-capture-streams))))

(defun ob-tmux-capture-stream-stop-all ()
  "Stop all active streaming captures."
  (interactive)
  (maphash (lambda (id _state)
             (ob-tmux-capture-stream-stop id "Manual stop"))
           ob-tmux-capture-streams)
  (message "All streaming captures stopped"))

(defun ob-tmux-capture-stream-list ()
  "List active streaming captures."
  (interactive)
  (let ((streams '()))
    (maphash (lambda (id state)
               (push (format "%s: %s (started: %s, finished: %s)"
                             id
                             (plist-get state :session)
                             (plist-get state :started)
                             (plist-get state :finished))
                     streams))
             ob-tmux-capture-streams)
    (if streams
        (message "Active streams:\n%s" (string-join streams "\n"))
      (message "No active streams"))))

;;; Asciinema Streaming Capture

(defvar ob-tmux-capture-asciinema-streams (make-hash-table :test 'equal)
  "Active asciinema streaming captures.")

(defvar ob-tmux-capture-asciinema-dir
  (expand-file-name "ob-tmux-casts" temporary-file-directory)
  "Directory for asciinema cast files.")

(defvar ob-tmux-capture-cast-stream-script
  (expand-file-name "cast-stream.py"
                    (file-name-directory (or load-file-name buffer-file-name "")))
  "Path to cast-stream.py helper script.")

(defun ob-tmux-capture-asciinema--ensure-dir ()
  "Ensure asciinema cast directory exists."
  (unless (file-directory-p ob-tmux-capture-asciinema-dir)
    (make-directory ob-tmux-capture-asciinema-dir t)))

(defun ob-tmux-capture-asciinema--read-cast-output (file)
  "Read and parse cast FILE using asciinema convert.
Returns the cleaned output text."
  (if (file-exists-p file)
      (let* ((txt-file (concat file ".txt"))
             (cmd (format "asciinema convert -f txt %s %s 2>/dev/null"
                          (shell-quote-argument file)
                          (shell-quote-argument txt-file))))
        (shell-command-to-string cmd)
        (if (file-exists-p txt-file)
            (prog1
                (with-temp-buffer
                  (insert-file-contents txt-file)
                  (buffer-string))
              (delete-file txt-file))
          ""))
    ""))

(defun ob-tmux-capture-execute-with-asciinema (body session socket &optional timeout params)
  "Execute BODY in SESSION with asciinema recording for capture.
Sends command to tmux wrapped in asciinema, tails the cast file.
If :stream-to is specified, also streams to remote server.
PARAMS is the org-babel params alist for metadata extraction."
  (let* ((id (format "asciinema-%s-%s" (float-time) (random 10000)))
         ;; Cast file goes to organized location (XDG or local)
         (cast-file (ob-tmux-capture--build-cast-path params))
         ;; Script file is transient, goes to temp
         (script-file (expand-file-name (concat id ".sh") temporary-file-directory))
         (timeout (or timeout ob-tmux-capture-timeout))
         (result-marker (point-marker))
         ;; Get metadata from ob-tmux-asciinema if available
         (title (when (fboundp 'ob-tmux-asciinema--build-title)
                  (ob-tmux-asciinema--build-title params)))
         (tags (when (fboundp 'ob-tmux-asciinema--build-tags)
                 (ob-tmux-asciinema--build-tags params)))
         (idle-limit (or (cdr (assq :idle-limit params)) 2))
         (capture-input (cdr (assq :capture-input params)))
         ;; Check if streaming to remote
         (stream-to (cdr (assq :stream-to params)))
         (stream-local (cdr (assq :stream-local params)))
         (streaming (or stream-to stream-local))
         ;; Write command to script file
         (_ (with-temp-file script-file
              (insert "#!/bin/bash\n")
              (insert body "\n")))
         ;; Build asciinema command - use 'session' if streaming, 'rec' otherwise
         (asciinema-cmd
          (if streaming
              ;; Streaming mode: use asciinema session
              (let* ((ws-url (when stream-to
                               (when (fboundp 'ob-tmux-asciinema--build-websocket-url)
                                 (let ((token (ob-tmux-asciinema-lookup-stream stream-to)))
                                   (ob-tmux-asciinema--build-websocket-url token params))))))
                (concat
                 "asciinema session --quiet"
                 (format " --output-file %s" (shell-quote-argument cast-file))
                 " --output-format asciicast-v3"
                 (when ws-url (format " --stream-remote %s" (shell-quote-argument ws-url)))
                 (when (and stream-local (not (eq stream-local t)))
                   (format " --stream-local 0.0.0.0:%s" stream-local))
                 (when (eq stream-local t) " --stream-local")
                 (when title (format " --title %s" (shell-quote-argument title)))
                 (when tags (format " --tags %s" (shell-quote-argument (mapconcat #'identity tags ","))))
                 (format " --idle-time-limit %s" idle-limit)
                 (when capture-input " --capture-input")
                 (format " --command 'bash %s'" (shell-quote-argument script-file))))
            ;; Recording only mode: use asciinema rec
            (concat
             "asciinema rec --quiet --overwrite"
             (when title (format " --title %s" (shell-quote-argument title)))
             (when tags (format " --tags %s" (shell-quote-argument (mapconcat #'identity tags ","))))
             (format " --idle-time-limit %s" idle-limit)
             (when capture-input " --capture-input")
             (format " --command 'bash %s' %s"
                     (shell-quote-argument script-file)
                     (shell-quote-argument cast-file)))))
         (state (list :id id
                      :streaming streaming
                      :cast-file cast-file
                      :script-file script-file
                      :session session
                      :socket socket
                      :result-marker result-marker
                      :buffer (current-buffer)
                      :start-time (float-time)
                      :timeout timeout
                      :last-content ""
                      :last-file-size nil
                      :finished nil
                      :timer nil
                      :params params)))
    ;; Store state
    (puthash id state ob-tmux-capture-asciinema-streams)
    ;; Insert initial empty results block
    (save-excursion
      (goto-char result-marker)
      (org-babel-insert-result "[asciinema recording...]" '("output" "replace")))
    ;; Send asciinema command to tmux
    (ob-tmux-capture--send-keys session socket asciinema-cmd)
    ;; Start polling timer to read cast file
    (let ((timer (run-with-timer
                  0.5  ; initial delay for asciinema to start
                  ob-tmux-capture-stream-interval
                  #'ob-tmux-capture-asciinema--tick id)))
      (plist-put state :timer timer))
    (message "Asciinema capture started: %s" id)
    ;; Return nil since results stream asynchronously
    nil))

(defun ob-tmux-capture-asciinema--tick (id)
  "Poll cast file for ID and update results."
  (cl-block ob-tmux-capture-asciinema--tick
    (let ((state (gethash id ob-tmux-capture-asciinema-streams)))
      (when (and state (not (plist-get state :finished)))
        (let* ((cast-file (plist-get state :cast-file))
               (result-marker (plist-get state :result-marker))
               (buf (plist-get state :buffer))
               (start-time (plist-get state :start-time))
               (timeout (plist-get state :timeout))
               (last-content (plist-get state :last-content))
               (last-file-size (plist-get state :last-file-size)))
          ;; Check timeout
          (when (> (- (float-time) start-time) timeout)
            (ob-tmux-capture-asciinema--stop id "Timeout")
            (cl-return-from ob-tmux-capture-asciinema--tick))
        ;; Read current content from cast file via asciinema convert
        (let ((current-content (ob-tmux-capture-asciinema--read-cast-output cast-file)))
          ;; Update results if content changed
          (when (and current-content
                     (not (string= current-content last-content)))
            (plist-put state :last-content current-content)
            (ob-tmux-capture-asciinema--update-result buf result-marker current-content))
          ;; Check if finished (file size stable for a while)
          (when (file-exists-p cast-file)
            (let ((current-size (file-attribute-size (file-attributes cast-file))))
              (if (and last-file-size
                       (= current-size last-file-size)
                       (> (- (float-time) start-time) 2))
                  ;; File size unchanged - check mod time
                  (let ((mod-time (float-time
                                   (file-attribute-modification-time
                                    (file-attributes cast-file)))))
                    (when (> (- (float-time) mod-time) 1.5)
                      (ob-tmux-capture-asciinema--stop id "Complete")))
                ;; Update tracked size
                (plist-put state :last-file-size current-size))))))))))

;; Note: ob-tmux-capture-asciinema--clean-output removed
;; asciinema convert -f txt already provides clean text output

(defun ob-tmux-capture-asciinema--update-result (buffer marker content)
  "Update results in BUFFER at MARKER with CONTENT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (when (re-search-forward "#\\+RESULTS:" nil t)
          (let ((results-line-end (line-end-position)))
            ;; Find the extent of the results block
            (forward-line 1)
            (let ((results-start (point))
                  (results-end (point)))
              ;; Find end: either next heading, next src block, or blank line + non-result
              (while (and (not (eobp))
                          (looking-at-p "^\\(: \\|#\\+\\|[[:space:]]*$\\)"))
                (forward-line 1))
              (setq results-end (point))
              ;; Delete old results content
              (delete-region results-start results-end)
              ;; Insert new content with proper formatting
              (goto-char results-start)
              (dolist (line (split-string content "\n"))
                (insert ": " line "\n")))))))))

;;; Cast Upload and History

(defun ob-tmux-capture--upload-cast (cast-file)
  "Upload CAST-FILE to asciinema server and return recording URL.
Returns nil if upload fails."
  (when (file-exists-p cast-file)
    (let* ((server-url (or (and (boundp 'ob-tmux-asciinema-server-url)
                                ob-tmux-asciinema-server-url)
                           (getenv "ASCIINEMA_SERVER_URL")))
           (cmd (concat "asciinema upload"
                        (when server-url
                          (format " --server-url %s" (shell-quote-argument server-url)))
                        " " (shell-quote-argument cast-file)))
           (output (shell-command-to-string cmd)))
      ;; Parse URL from output like "View the recording at:\n\n    https://..."
      (when (string-match "\\(https?://[^ \n\t]+\\)" output)
        (match-string 1 output)))))

(defun ob-tmux-capture--get-cast-duration (cast-file)
  "Get duration of CAST-FILE in seconds."
  (when (file-exists-p cast-file)
    (let* ((cmd (format "tail -1 %s | jq -r '.[0]'" (shell-quote-argument cast-file)))
           (output (string-trim (shell-command-to-string cmd))))
      (when (string-match "^[0-9.]+$" output)
        (string-to-number output)))))

(defun ob-tmux-capture--find-history-table (buffer)
  "Find or create #+CAST_HISTORY table position in BUFFER.
Returns marker at position to insert new row, or nil."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+CAST_HISTORY:" nil t)
          ;; Found existing table - find end of table
          (progn
            (forward-line 1)
            ;; Skip header rows
            (while (and (looking-at "^|") (not (eobp)))
              (forward-line 1))
            (point-marker))
        ;; No table - create one at end of preamble
        (goto-char (point-min))
        ;; Skip to end of property/keyword section
        (while (and (looking-at "^\\(#\\+\\|$\\)") (not (eobp)))
          (forward-line 1))
        ;; Insert new table
        (insert "\n#+CAST_HISTORY:\n")
        (insert "| Timestamp | Recording URL | Duration |\n")
        (insert "|-----------|---------------|----------|\n")
        (point-marker)))))

(defun ob-tmux-capture--add-to-history (buffer recording-url duration)
  "Add RECORDING-URL with DURATION to cast history in BUFFER."
  (when (and buffer (buffer-live-p buffer) recording-url)
    (let ((timestamp (format-time-string "%Y-%m-%d %H:%M"))
          (duration-str (if duration (format "%.1fs" duration) "-")))
      (with-current-buffer buffer
        (save-excursion
          (let ((pos (ob-tmux-capture--find-history-table buffer)))
            (when pos
              (goto-char pos)
              (insert (format "| %s | %s | %s |\n"
                              timestamp recording-url duration-str)))))))))

(defun ob-tmux-capture-asciinema--stop (id &optional reason)
  "Stop asciinema capture ID and cleanup."
  (let ((state (gethash id ob-tmux-capture-asciinema-streams)))
    (when state
      (let ((timer (plist-get state :timer))
            (cast-file (plist-get state :cast-file))
            (script-file (plist-get state :script-file))
            (buf (plist-get state :buffer))
            (result-marker (plist-get state :result-marker))
            (params (plist-get state :params)))
        (when timer (cancel-timer timer))
        ;; Upload and add to CAST_LOG drawer if enabled
        (when (and ob-tmux-capture-auto-upload
                   cast-file
                   (file-exists-p cast-file))
          (message "Uploading cast file...")
          (let ((recording-url (ob-tmux-capture--upload-cast cast-file))
                (duration (ob-tmux-capture--get-cast-duration cast-file)))
            (when recording-url
              (message "Recording URL: %s" recording-url)
              ;; Use per-block CAST_LOG drawer from ob-tmux-asciinema
              (when (and (buffer-live-p buf)
                         (fboundp 'ob-tmux-asciinema-record-execution))
                (with-current-buffer buf
                  (save-excursion
                    (goto-char result-marker)
                    (ob-tmux-asciinema-record-execution
                     (list :cast-url recording-url
                           :cast-file cast-file
                           :duration duration)
                     params)))))))
        ;; Cleanup script file (always transient)
        (run-with-timer 5 nil
                        (lambda (sf) (ignore-errors (delete-file sf)))
                        script-file)
        ;; Cast files are deleted by default (remote URL is the record)
        ;; Keep only if :keep-cast is explicitly "yes"
        (unless (member (cdr (assq :keep-cast params)) '("yes" yes t))
          (run-with-timer 5 nil
                          (lambda (cf)
                            (ignore-errors (delete-file cf))
                            ;; Also clean up empty parent dirs
                            (ignore-errors
                              (let ((dir (file-name-directory cf)))
                                (when (and dir (directory-empty-p dir))
                                  (delete-directory dir)))))
                          cast-file)))
      (plist-put state :finished t)
      (message "Asciinema capture %s: %s" id (or reason "Stopped"))
      (run-with-timer 5 nil #'remhash id ob-tmux-capture-asciinema-streams))))

;;; Main Entry Point

(defun ob-tmux-capture--filter-output (output filter-lines grep-pattern)
  "Filter OUTPUT based on FILTER-LINES and GREP-PATTERN.
FILTER-LINES: positive = first N lines, negative = last N lines.
GREP-PATTERN: regex to match lines against."
  (when output
    (let ((lines (split-string output "\n" t)))  ; t = omit nulls
      ;; Apply grep filter first
      (when grep-pattern
        (setq lines (seq-filter
                     (lambda (line) (string-match-p grep-pattern line))
                     lines)))
      ;; Apply lines filter (positive = head, negative = tail)
      (when filter-lines
        (if (> filter-lines 0)
            (setq lines (seq-take lines filter-lines))
          (setq lines (seq-drop lines (max 0 (+ (length lines) filter-lines))))))
      (string-join lines "\n"))))

(defun ob-tmux-capture-execute (body params)
  "Execute BODY with output capture based on PARAMS.
PARAMS is an alist of header arguments including:
  :session    - tmux session name (required)
  :socket     - tmux socket path (optional)
  :capture    - capture method: pane, markers, stream, asciinema (optional)
  :timeout    - timeout in seconds (optional)
  :scrollback - for pane capture: lines of history to grab (optional)
  :lines      - output filter: positive=first N, negative=last N (optional)
  :grep       - regex pattern to filter output lines (optional)
  :raw        - if yes, include prompts and command echoes (default: clean)"
  (let* ((session (cdr (assq :session params)))
         (socket (cdr (assq :socket params)))
         (capture (or (cdr (assq :capture params))
                      ob-tmux-capture-default-method))
         (timeout (or (cdr (assq :timeout params))
                      ob-tmux-capture-timeout))
         (raw (cdr (assq :raw params)))
         (filter-lines (cdr (assq :lines params)))
         (grep-pattern (cdr (assq :grep params))))
    (unless session
      (error "ob-tmux-capture: :session is required"))
    (let ((output
           (pcase capture
             ((or 'pane "pane")
              ;; Simple pane capture - :scrollback for history amount
              (let* ((scrollback (cdr (assq :scrollback params)))
                     (pane-raw (ob-tmux-capture--capture-pane session socket scrollback)))
                (ob-tmux-capture--clean-output pane-raw)))
             ((or 'markers "markers")
              (ob-tmux-capture-execute-with-markers body session socket timeout raw))
             ((or 'stream "stream")
              ;; Streaming is async - returns stream ID, not output
              (ob-tmux-capture-stream-start body session socket timeout)
              nil)  ; Return nil since results are streamed
             ((or 'asciinema "asciinema")
              (ob-tmux-capture-execute-with-asciinema body session socket timeout params))
             (_ (error "ob-tmux-capture: Unknown capture method: %s" capture)))))
      ;; Apply filters if specified
      (if (or filter-lines grep-pattern)
          (ob-tmux-capture--filter-output output filter-lines grep-pattern)
        output))))

;;; Interactive Commands

(defun ob-tmux-capture-pane-at-point ()
  "Capture current tmux pane and insert as #+RESULTS: block.
Uses session/socket from the source block at point."
  (interactive)
  (let* ((info (org-babel-get-src-block-info))
         (params (nth 2 info))
         (session (cdr (assq :session params)))
         (socket (cdr (assq :socket params))))
    (unless session
      (error "No :session found in source block"))
    (let ((output (ob-tmux-capture--capture-pane session socket)))
      (setq output (ob-tmux-capture--clean-output output))
      (org-babel-insert-result output '("output" "verbatim"))
      (message "Captured %d lines" (length (split-string output "\n"))))))

(defun ob-tmux-capture-execute-at-point ()
  "Execute tmux block at point with output capture."
  (interactive)
  (let* ((info (org-babel-get-src-block-info))
         (body (nth 1 info))
         (params (nth 2 info))
         ;; Force capture mode
         (params-with-capture (cons '(:capture . markers) params)))
    (let ((output (ob-tmux-capture-execute body params-with-capture)))
      (if output
          (progn
            (org-babel-insert-result output '("output" "verbatim"))
            (message "Captured %d lines" (length (split-string output "\n"))))
        (message "No output captured (timeout?)")))))

;;; Integration with ob-tmux (Optional)

(defun ob-tmux-capture-advice-execute (orig-fun body params)
  "Advice for `org-babel-execute:tmux' to add capture support.
Only activates if :capture is specified in PARAMS.
When capture is active, we handle result insertion ourselves since
ob-tmux forces :results silent."
  (let ((capture (cdr (assq :capture params))))
    (if capture
        ;; Use capture mode - insert result directly since ob-tmux forces silent
        (let ((output (ob-tmux-capture-execute body params)))
          (when output
            ;; Insert result ourselves, bypassing org-babel's silent handling
            (org-babel-insert-result output '("output" "replace")))
          ;; Return nil so org-babel doesn't try to insert again
          nil)
      ;; Fall through to original
      (funcall orig-fun body params))))

(defun ob-tmux-capture-enable-advice ()
  "Enable automatic capture for ob-tmux blocks with :capture header."
  (interactive)
  (advice-add 'org-babel-execute:tmux :around #'ob-tmux-capture-advice-execute)
  (advice-add 'org-babel-execute:tmate :around #'ob-tmux-capture-advice-execute)
  (message "ob-tmux-capture advice enabled"))

(defun ob-tmux-capture-disable-advice ()
  "Disable automatic capture advice."
  (interactive)
  (advice-remove 'org-babel-execute:tmux #'ob-tmux-capture-advice-execute)
  (advice-remove 'org-babel-execute:tmate #'ob-tmux-capture-advice-execute)
  (message "ob-tmux-capture advice disabled"))

;;; Keybindings (suggested)

;; (define-key org-mode-map (kbd "C-c t c") #'ob-tmux-capture-pane-at-point)
;; (define-key org-mode-map (kbd "C-c t x") #'ob-tmux-capture-execute-at-point)

(provide 'ob-tmux-capture)
;;; ob-tmux-capture.el ends here
