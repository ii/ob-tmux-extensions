;;; ob-tmux-asciinema.el --- Asciinema configuration for ob-tmux -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: ii.coop
;; Keywords: org, tmux, asciinema, literate programming
;; Version: 0.1.0

;;; Commentary:

;; This library provides asciinema configuration and metadata mapping for
;; org-mode tmux blocks. It integrates with ob-tmux-capture to provide:
;;
;; - Cast file storage and management
;; - Stream metadata from org context (title, tags, etc.)
;; - Live streaming configuration
;; - Replay and scrobbling support via properties
;;
;; Usage:
;;   #+begin_src tmux :session demo:kubectl :capture asciinema
;;   kubectl get pods
;;   #+end_src
;;
;; Header arguments:
;;   :capture asciinema     - Enable asciinema capture
;;   :title "My Title"      - Stream/recording title (default: from #+TITLE or heading)
;;   :tags "k8s,tutorial"   - Additional tags (merged with #+FILETAGS)
;;   :stream-to ID          - Stream to remote server with stream ID
;;   :stream-local PORT     - Start local streaming server
;;   :capture-input t       - Record keyboard input
;;   :idle-limit SECS       - Cap idle time
;;   :cast-dir DIR          - Directory for cast files
;;   :keep-cast t           - Keep cast file after execution
;;
;; After execution, properties are added to the block:
;;   #+CAST: /path/to/file.cast
;;   #+STREAM_URL: https://server/s/token
;;   #+DURATION: 5.2

;;; Code:

(require 'org)
(require 'org-element)
(require 'json)
(require 'cl-lib)

;;; Customization

(defgroup ob-tmux-asciinema nil
  "Asciinema configuration for ob-tmux."
  :group 'org-babel
  :prefix "ob-tmux-asciinema-")

(defcustom ob-tmux-asciinema-binary "asciinema"
  "Path to asciinema binary."
  :type 'string
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-cast-dir
  (expand-file-name "casts" user-emacs-directory)
  "Default directory for storing cast files.
Can be overridden per-file with #+PROPERTY: cast-dir or per-block with :cast-dir."
  :type 'directory
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-server-url nil
  "Default asciinema server URL for streaming.
Example: \"https://asciinema.example.com\"
Can be overridden with ASCIINEMA_SERVER_URL env var or :server-url header."
  :type '(choice (const nil) string)
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-default-stream-id nil
  "Default stream ID to use for streaming.
Can be set per-file with #+PROPERTY: stream-id or per-block with :stream-to."
  :type '(choice (const nil) string)
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-capture-input nil
  "Whether to capture keyboard input by default.
WARNING: This captures passwords and sensitive input.
Can be overridden per-block with :capture-input."
  :type 'boolean
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-idle-time-limit 2.0
  "Default idle time limit in seconds.
Can be overridden per-block with :idle-limit."
  :type 'number
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-auto-tags t
  "Whether to automatically generate tags from context.
When non-nil, adds tags for org file, tmux session, window, etc."
  :type 'boolean
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-keep-cast t
  "Whether to keep cast files after execution by default.
Can be overridden per-block with :keep-cast."
  :type 'boolean
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-stream-registry nil
  "Registry mapping friendly names to stream producer tokens.
Format: ((\"name\" . \"producer_token\") ...)
Can be set via customization or loaded from a file.

Example:
  ((\"tinkerbell\" . \"jYf5K9MpDqR3xZ2L\")
   (\"demo\" . \"aB7cD4eF9gH2iJ5k\"))"
  :type '(alist :key-type string :value-type string)
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-stream-registry-file nil
  "Path to JSON file containing stream registry.
If set, registry is loaded from this file.
Format: {\"streams\": {\"name\": \"token\", ...}}"
  :type '(choice (const nil) file)
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-install-id-file
  (expand-file-name "~/.local/state/asciinema/install-id")
  "Path to asciinema install-id file for API authentication."
  :type 'file
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-cast-url-timeout 5
  "Timeout in seconds waiting for cast_url to become available."
  :type 'integer
  :group 'ob-tmux-asciinema)

;;; Server API Client
;;
;; HTTP client for direct communication with asciinema server.
;; Works with both vanilla asciinema and servers with cast_url support.

(require 'url)

(defun ob-tmux-asciinema--get-install-id ()
  "Read the asciinema install-id from file for API auth."
  (let ((file ob-tmux-asciinema-install-id-file))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (string-trim (buffer-string)))
      nil)))

(defun ob-tmux-asciinema-api-call (method endpoint &optional data)
  "Make an API call to asciinema server.
METHOD is GET, POST, PATCH, or DELETE.
ENDPOINT is the API path (e.g., \"/api/v1/streams\").
DATA is an alist to send as JSON body.
Returns parsed JSON response or nil on error.
Works with any asciinema server (vanilla or modified)."
  (let ((install-id (ob-tmux-asciinema--get-install-id)))
    (unless install-id
      (error "Asciinema not authenticated. Run 'asciinema auth' first"))
    (let* ((url-request-method method)
           (auth-string (base64-encode-string
                         (format "%s:%s" (user-login-name) install-id) t))
           (url-request-extra-headers
            `(("Authorization" . ,(format "Basic %s" auth-string))
              ("Accept" . "application/json")
              ("Content-Type" . "application/json")))
           (url-request-data (when data (encode-coding-string
                                         (json-encode data) 'utf-8)))
           (server-url (or ob-tmux-asciinema-server-url
                           (getenv "ASCIINEMA_SERVER_URL")))
           (url (concat server-url endpoint))
           (buffer (condition-case nil
                       (url-retrieve-synchronously url t t 30)
                     (error nil))))
      (when buffer
        (unwind-protect
            (with-current-buffer buffer
              (goto-char (point-min))
              ;; Skip HTTP headers
              (when (re-search-forward "^\r?\n" nil t)
                (let ((json-object-type 'alist)
                      (json-array-type 'list)
                      (json-key-type 'symbol))
                  (condition-case nil
                      (json-read)
                    (error nil)))))
          (kill-buffer buffer))))))

(defun ob-tmux-asciinema-api-get-stream (stream-id)
  "Get stream info by STREAM-ID from server.
Returns alist with id, url, ws_producer_url, and optionally cast_url."
  (ob-tmux-asciinema-api-call "GET" (format "/api/v1/streams/%s" stream-id)))

(defun ob-tmux-asciinema-api-list-streams (&optional prefix)
  "List user's streams from server, optionally filtered by PREFIX."
  (let ((endpoint (if prefix
                      (format "/api/v1/user/streams?limit=200&prefix=%s"
                              (url-hexify-string prefix))
                    "/api/v1/user/streams?limit=200")))
    (ob-tmux-asciinema-api-call "GET" endpoint)))

(defun ob-tmux-asciinema-api-create-stream (&optional title tags)
  "Create a new stream on server with optional TITLE and TAGS."
  (let ((data `((live . t))))
    (when title (push `(title . ,title) data))
    (when tags (push `(tags . ,(vconcat tags)) data))
    (ob-tmux-asciinema-api-call "POST" "/api/v1/streams" data)))

(defun ob-tmux-asciinema-api-update-stream (stream-id &rest attrs)
  "Update stream STREAM-ID with ATTRS on server.
ATTRS is a plist like :title \"foo\" :tags (\"a\" \"b\")."
  (let ((data '()))
    (while attrs
      (let ((key (pop attrs))
            (val (pop attrs)))
        (push (cons (intern (substring (symbol-name key) 1)) val) data)))
    (ob-tmux-asciinema-api-call "PATCH"
                                (format "/api/v1/streams/%s" stream-id)
                                data)))

(defun ob-tmux-asciinema-api-find-stream-by-title (title)
  "Find a stream by exact TITLE match on server."
  (let ((streams (ob-tmux-asciinema-api-list-streams)))
    (cl-find-if (lambda (s) (equal (alist-get 'title s) title))
                streams)))

(defun ob-tmux-asciinema-api-get-or-create-stream (name &optional tags)
  "Get existing stream by NAME or create new one on server."
  (or (ob-tmux-asciinema-api-find-stream-by-title name)
      (ob-tmux-asciinema-api-create-stream name tags)))

;;; Cast URL (recording URL) - backwards compatible
;;
;; These functions work with both:
;; 1. Modified servers (cast_url available when streaming starts)
;; 2. Vanilla upstream asciinema (cast_url not supported, returns nil)

(defvar ob-tmux-asciinema--server-supports-cast-url 'unknown
  "Whether current server supports cast_url feature.
Values: 'unknown, t, or nil. Reset when server URL changes.")

(defvar ob-tmux-asciinema--server-url-for-cast-check nil
  "Server URL when cast_url support was last checked.")

(defun ob-tmux-asciinema--reset-cast-url-support-cache ()
  "Reset the cast_url support cache if server URL changed."
  (let ((current-url (or ob-tmux-asciinema-server-url
                         (getenv "ASCIINEMA_SERVER_URL"))))
    (unless (equal current-url ob-tmux-asciinema--server-url-for-cast-check)
      (setq ob-tmux-asciinema--server-supports-cast-url 'unknown)
      (setq ob-tmux-asciinema--server-url-for-cast-check current-url))))

(defun ob-tmux-asciinema-api-get-cast-url (stream-id)
  "Get cast_url for STREAM-ID if server supports it and stream is active.
Returns URL string or nil. Works with vanilla servers (returns nil).
Caches whether server supports this feature to avoid repeated checks."
  (ob-tmux-asciinema--reset-cast-url-support-cache)
  ;; If we know server doesn't support cast_url, skip the API call
  (unless (eq ob-tmux-asciinema--server-supports-cast-url nil)
    (let ((stream (ob-tmux-asciinema-api-get-stream stream-id)))
      (when stream
        (let ((cast-url (alist-get 'cast_url stream)))
          ;; Update our knowledge of server support
          ;; If response has cast_url key (even if nil), server supports it
          ;; We check if the key exists, not just if it has a value
          (when (eq ob-tmux-asciinema--server-supports-cast-url 'unknown)
            (setq ob-tmux-asciinema--server-supports-cast-url
                  (if (assq 'cast_url stream) t nil)))
          cast-url)))))

(defun ob-tmux-asciinema-api-wait-for-cast-url (stream-id &optional timeout)
  "Wait for cast_url to become available for STREAM-ID.
TIMEOUT defaults to `ob-tmux-asciinema-cast-url-timeout'.
Returns cast_url string or nil if timeout or unsupported server.

On vanilla servers without cast_url support, returns nil immediately
after first check (no unnecessary polling)."
  (ob-tmux-asciinema--reset-cast-url-support-cache)
  ;; If we already know server doesn't support cast_url, return nil immediately
  (if (eq ob-tmux-asciinema--server-supports-cast-url nil)
      nil
    (let ((timeout (or timeout ob-tmux-asciinema-cast-url-timeout))
          (start-time (float-time))
          (cast-url nil)
          (keep-trying t))
      (while (and keep-trying
                  (not cast-url)
                  (< (- (float-time) start-time) timeout))
        (setq cast-url (ob-tmux-asciinema-api-get-cast-url stream-id))
        ;; Stop polling if we learned server doesn't support cast_url
        (when (eq ob-tmux-asciinema--server-supports-cast-url nil)
          (setq keep-trying nil))
        (when (and keep-trying (not cast-url))
          (sleep-for 0.5)))
      cast-url)))

(defun ob-tmux-asciinema-server-supports-cast-url-p ()
  "Return t if current server supports pre-allocated cast_url feature.
Returns nil for vanilla upstream servers.
Returns 'unknown if not yet determined (no API call made yet)."
  (ob-tmux-asciinema--reset-cast-url-support-cache)
  ob-tmux-asciinema--server-supports-cast-url)

;;; High-level integration helpers
;;
;; Usage flow for both server types:
;;
;; Modified server (with cast_url):
;;   1. (ob-tmux-asciinema-start-stream-session "name") -> stream-info
;;   2. Start asciinema with :ws-producer-url
;;   3. (ob-tmux-asciinema-get-session-cast-url stream-id) -> cast_url (immediate)
;;   4. Display cast_url to user - they know recording URL now
;;
;; Vanilla server (without cast_url):
;;   1. (ob-tmux-asciinema-start-stream-session "name") -> stream-info
;;   2. Start asciinema with :ws-producer-url
;;   3. (ob-tmux-asciinema-get-session-cast-url stream-id) -> nil (returns quickly)
;;   4. Recording URL only available after stream ends (check stream page)

(defun ob-tmux-asciinema-start-stream-session (name &optional tags)
  "Start streaming session for NAME, return plist with stream info.
Creates stream if needed. After asciinema connects, call
`ob-tmux-asciinema-get-session-cast-url' to get recording URL.

Works with any asciinema server. On servers without cast_url support,
recording URL won't be available until after stream ends.

Returns plist:
  :stream-id      - Numeric stream ID
  :ws-producer-url - WebSocket URL for asciinema --stream-remote
  :view-url       - Public URL to watch the stream
  :title          - Stream title"
  (when-let ((stream (ob-tmux-asciinema-api-get-or-create-stream name tags)))
    (let ((stream-id (alist-get 'id stream))
          (ws-url (alist-get 'ws_producer_url stream))
          (view-url (alist-get 'url stream)))
      (list :stream-id stream-id
            :ws-producer-url ws-url
            :view-url view-url
            :title name))))

(defun ob-tmux-asciinema-get-session-cast-url (stream-id)
  "Get cast_url for active stream STREAM-ID.
Call this after asciinema has connected via WebSocket.

On modified servers: Returns recording URL (e.g., /a/xyz123) immediately.
On vanilla servers: Returns nil quickly (no unnecessary polling).

The cast_url is the permanent link to the recording that will exist
after the stream ends."
  (let ((cast-url (ob-tmux-asciinema-api-wait-for-cast-url stream-id)))
    (if cast-url
        (message "Recording will be at: %s" cast-url)
      (when (eq ob-tmux-asciinema--server-supports-cast-url nil)
        (message "Server doesn't support pre-allocated cast URLs. Recording URL available after stream ends.")))
    cast-url))

(defun ob-tmux-asciinema-format-result-links (stream-info &optional cast-url)
  "Format STREAM-INFO and CAST-URL as org properties.
Returns string suitable for insertion after #+RESULTS:."
  (let ((view-url (plist-get stream-info :view-url))
        (stream-id (plist-get stream-info :stream-id)))
    (concat
     (when view-url (format "#+STREAM_URL: %s\n" view-url))
     (when stream-id (format "#+STREAM_ID: %s\n" stream-id))
     (when cast-url (format "#+CAST_URL: %s\n" cast-url)))))

(defun ob-tmux-asciinema-get-recording-url-after-stream (stream-id)
  "Get recording URL after stream has ended (for vanilla servers).
Fetches the stream page and extracts the most recent recording link.
Returns URL string or nil if no recordings found."
  (let* ((stream (ob-tmux-asciinema-api-get-stream stream-id))
         (view-url (when stream (alist-get 'url stream))))
    (when view-url
      ;; Fetch stream page HTML and look for recording links
      (let ((url-request-method "GET")
            (buffer (condition-case nil
                        (url-retrieve-synchronously view-url t t 10)
                      (error nil))))
        (when buffer
          (unwind-protect
              (with-current-buffer buffer
                (goto-char (point-min))
                ;; Look for /a/TOKEN links
                (when (re-search-forward "href=\"\\(/a/[^\"]+\\)\"" nil t)
                  (let ((server-url (or ob-tmux-asciinema-server-url
                                        (getenv "ASCIINEMA_SERVER_URL"))))
                    (concat server-url (match-string 1)))))
            (kill-buffer buffer)))))))

;;; Stream Registry

(defvar ob-tmux-asciinema--stream-cache nil
  "Cached stream registry from file.")

(defvar ob-tmux-asciinema--stream-cache-mtime nil
  "Modification time when cache was last loaded.")

(defun ob-tmux-asciinema--load-registry-file ()
  "Load stream registry from JSON file if configured."
  (when ob-tmux-asciinema-stream-registry-file
    (let ((file ob-tmux-asciinema-stream-registry-file))
      (when (file-exists-p file)
        (let ((mtime (file-attribute-modification-time (file-attributes file))))
          ;; Only reload if file changed
          (unless (and ob-tmux-asciinema--stream-cache
                       ob-tmux-asciinema--stream-cache-mtime
                       (equal mtime ob-tmux-asciinema--stream-cache-mtime))
            (condition-case err
                (let* ((json-object-type 'alist)
                       (json-array-type 'list)
                       (data (json-read-file file))
                       (streams (cdr (assq 'streams data))))
                  (setq ob-tmux-asciinema--stream-cache
                        (mapcar (lambda (pair)
                                  (cons (symbol-name (car pair)) (cdr pair)))
                                streams))
                  (setq ob-tmux-asciinema--stream-cache-mtime mtime))
              (error
               (message "ob-tmux-asciinema: Error loading registry: %s" err)))))))))

(defun ob-tmux-asciinema--get-stream-registry ()
  "Get the combined stream registry (custom + file)."
  (ob-tmux-asciinema--load-registry-file)
  (append ob-tmux-asciinema-stream-registry
          ob-tmux-asciinema--stream-cache))

(defun ob-tmux-asciinema-add-stream (name token)
  "Add or update stream NAME with TOKEN in the registry."
  (interactive "sStream name: \nsProducer token: ")
  (let ((existing (assoc name ob-tmux-asciinema-stream-registry)))
    (if existing
        (setcdr existing token)
      (push (cons name token) ob-tmux-asciinema-stream-registry))
    (message "Added stream: %s" name)))

;;; Multi-level Stream Configuration
;;
;; Configuration levels (highest priority first):
;; 1. Block-level: :stream-to header arg
;; 2. File-level: #+STREAM: name token
;; 3. File-level: #+PROPERTY: stream-registry-file path
;; 4. Directory-level: .dir-locals.el setting ob-tmux-asciinema-stream-registry
;; 5. Global: customize ob-tmux-asciinema-stream-registry

(defun ob-tmux-asciinema--get-file-streams ()
  "Get stream mappings defined in current file with #+STREAM: keywords.
Returns alist of (name . token) pairs."
  (let ((streams '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+STREAM:[ \t]+\\([^ \t\n]+\\)[ \t]+\\([^ \t\n]+\\)" nil t)
        (push (cons (match-string 1) (match-string 2)) streams)))
    (nreverse streams)))

;;; Pane → Stream Mappings
;;
;; Map tmux pane identifiers (session:window) to stream names.
;; Format: #+PANE_STREAM: session:window stream-name [stream-url]

(defun ob-tmux-asciinema--get-file-pane-streams ()
  "Get pane → stream mappings defined in current file.
Looks for #+PANE_STREAM: keywords.
Format: #+PANE_STREAM: session:window stream-name [stream-url]
Returns alist of (pane . (name . url)) pairs."
  (let ((mappings '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^#\\+PANE_STREAM:[ \t]+\\([^ \t\n]+\\)[ \t]+\\([^ \t\n]+\\)\\(?:[ \t]+\\([^ \t\n]+\\)\\)?"
              nil t)
        (let ((pane (match-string 1))
              (stream-name (match-string 2))
              (stream-url (match-string 3)))
          (push (cons pane (cons stream-name stream-url)) mappings))))
    (nreverse mappings)))

(defun ob-tmux-asciinema-get-pane-stream-config (pane)
  "Get configured stream for PANE from org file.
PANE is a string like \"session:window\".
Returns (name . url) or nil if not configured."
  (let ((mappings (ob-tmux-asciinema--get-file-pane-streams)))
    (cdr (assoc pane mappings))))

(defun ob-tmux-asciinema-add-pane-stream-keyword (pane stream-name &optional stream-url)
  "Add #+PANE_STREAM: keyword for PANE with STREAM-NAME and optional STREAM-URL.
Adds to the file header area after any existing keywords."
  (save-excursion
    (goto-char (point-min))
    ;; Find end of keyword block (before first heading or content)
    (let ((insert-point
           (if (re-search-forward "^\\*\\|^[^#\n]" nil t)
               (match-beginning 0)
             (point-max))))
      ;; Check if this pane already has a mapping
      (goto-char (point-min))
      (if (re-search-forward
           (format "^#\\+PANE_STREAM:[ \t]+%s[ \t]" (regexp-quote pane))
           insert-point t)
          ;; Update existing
          (progn
            (beginning-of-line)
            (delete-region (point) (line-end-position))
            (insert (format "#+PANE_STREAM: %s %s%s"
                            pane stream-name
                            (if stream-url (format " %s" stream-url) ""))))
        ;; Insert new
        (goto-char insert-point)
        (unless (bolp) (insert "\n"))
        (insert (format "#+PANE_STREAM: %s %s%s\n"
                        pane stream-name
                        (if stream-url (format " %s" stream-url) "")))))))

(defun ob-tmux-asciinema-remove-pane-stream-keyword (pane)
  "Remove #+PANE_STREAM: keyword for PANE from current file."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^#\\+PANE_STREAM:[ \t]+%s[ \t].*\n?" (regexp-quote pane))
           nil t)
      (replace-match ""))))

(defun ob-tmux-asciinema--get-file-registry-path ()
  "Get stream registry file path from #+PROPERTY: stream-registry-file."
  (ob-tmux-asciinema--get-file-property "stream-registry-file"))

(defvar-local ob-tmux-asciinema--buffer-registry-cache nil
  "Buffer-local cache of combined stream registry.")

(defvar-local ob-tmux-asciinema--buffer-registry-tick nil
  "Buffer modification tick when cache was created.")

(defun ob-tmux-asciinema--get-combined-registry ()
  "Get combined stream registry from all configuration levels.
Returns alist with file-level definitions taking priority."
  ;; Check cache validity
  (let ((current-tick (buffer-modified-tick)))
    (unless (and ob-tmux-asciinema--buffer-registry-cache
                 (eq ob-tmux-asciinema--buffer-registry-tick current-tick))
      ;; Rebuild cache
      (let ((registry '()))
        ;; Level 5: Global customization
        (setq registry (append registry ob-tmux-asciinema-stream-registry))
        ;; Level 4: Global file (already in registry via load)
        (setq registry (append registry ob-tmux-asciinema--stream-cache))
        ;; Level 3: File-level registry file
        (when-let ((file-registry-path (ob-tmux-asciinema--get-file-registry-path)))
          (let ((expanded (expand-file-name file-registry-path
                                           (file-name-directory (or buffer-file-name "")))))
            (when (file-exists-p expanded)
              (condition-case nil
                  (let* ((json-object-type 'alist)
                         (json-array-type 'list)
                         (data (json-read-file expanded))
                         (streams (cdr (assq 'streams data))))
                    (dolist (pair streams)
                      (push (cons (symbol-name (car pair)) (cdr pair)) registry)))
                (error nil)))))
        ;; Level 2: File-level #+STREAM: keywords (highest priority)
        (setq registry (append (ob-tmux-asciinema--get-file-streams) registry))
        ;; Remove duplicates (first occurrence wins)
        (setq registry (cl-remove-duplicates registry :key #'car :test #'string= :from-end t))
        ;; Cache it
        (setq ob-tmux-asciinema--buffer-registry-cache registry)
        (setq ob-tmux-asciinema--buffer-registry-tick current-tick)))
    ob-tmux-asciinema--buffer-registry-cache))

(defun ob-tmux-asciinema--looks-like-token-p (str)
  "Check if STR looks like a raw token that could be used directly.
Returns t for:
  - UUID format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
  - Alphanumeric strings 12+ chars (likely tokens, not friendly names)"
  (or
   ;; UUID format (with hyphens)
   (string-match-p "^[0-9a-fA-F]\\{8\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{4\\}-[0-9a-fA-F]\\{12\\}$" str)
   ;; Long alphanumeric token (12+ chars suggests it's a token, not a name)
   (and (string-match-p "^[a-zA-Z0-9]+$" str)
        (>= (length str) 12))))

;;; Auto-create stream cache (session-level persistence)

(defvar ob-tmux-asciinema--session-stream-cache nil
  "Session-level cache of stream name -> info mappings.
Stores streams that were auto-created or fetched from API.
Format: ((\"name\" . (:id N :ws-producer-url URL :producer-token TOKEN)) ...)")

(defcustom ob-tmux-asciinema-auto-create-streams t
  "Whether to automatically create streams on server when not found locally.
When non-nil, if a stream name isn't in the registry, the server is queried
and streams are created on-demand."
  :type 'boolean
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-persist-auto-created-streams 'prompt
  "How to persist auto-created stream mappings.
Values:
  nil       - Don't persist (session cache only)
  'file     - Auto-add #+STREAM: to current org file (committed)
  'dir-local - Add to .dir-locals-2.el (personal, not committed)
  'global   - Auto-add to global registry file
  'prompt   - Ask user each time"
  :type '(choice (const :tag "Don't persist" nil)
                 (const :tag "Add to org file" file)
                 (const :tag "Add to .dir-locals-2.el" dir-local)
                 (const :tag "Add to global registry" global)
                 (const :tag "Ask each time" prompt))
  :group 'ob-tmux-asciinema)

(defun ob-tmux-asciinema--cache-stream (name stream-info)
  "Cache STREAM-INFO for NAME in session cache."
  (let ((existing (assoc name ob-tmux-asciinema--session-stream-cache)))
    (if existing
        (setcdr existing stream-info)
      (push (cons name stream-info) ob-tmux-asciinema--session-stream-cache))))

(defun ob-tmux-asciinema--get-cached-stream (name)
  "Get cached stream info for NAME, or nil."
  (cdr (assoc name ob-tmux-asciinema--session-stream-cache)))

(defun ob-tmux-asciinema--extract-producer-token (ws-url)
  "Extract producer token from WS-URL like wss://host/ws/S/TOKEN."
  (when (string-match "/ws/S/\\([^/]+\\)$" ws-url)
    (match-string 1 ws-url)))

(defun ob-tmux-asciinema--persist-stream-to-file (name token)
  "Add #+STREAM: NAME TOKEN to current org file."
  (save-excursion
    (goto-char (point-min))
    ;; Find good insertion point (after other #+STREAM: or file headers)
    (let ((insert-point (point-min)))
      (while (looking-at "^#\\+\\(TITLE\\|AUTHOR\\|DATE\\|FILETAGS\\|PROPERTY\\|STREAM\\):")
        (forward-line 1)
        (setq insert-point (point)))
      (goto-char insert-point)
      (insert (format "#+STREAM: %s %s\n" name token))
      (message "Added #+STREAM: %s to file" name))))

(defun ob-tmux-asciinema--persist-stream-to-dir-local (name token)
  "Add stream NAME -> TOKEN to .dir-locals-2.el (personal, not committed)."
  (let* ((dir (or (and buffer-file-name (file-name-directory buffer-file-name))
                  default-directory))
         (file (expand-file-name ".dir-locals-2.el" dir)))
    ;; Read existing .dir-locals-2.el or start fresh
    (let* ((existing (when (file-exists-p file)
                       (with-temp-buffer
                         (insert-file-contents file)
                         (condition-case nil
                             (read (current-buffer))
                           (error nil)))))
           ;; Find or create the nil (all modes) entry
           (nil-entry (or (assq nil existing) (cons nil nil)))
           (nil-settings (cdr nil-entry))
           ;; Find or create the stream-registry setting
           (registry-entry (or (assq 'ob-tmux-asciinema-stream-registry nil-settings)
                               (cons 'ob-tmux-asciinema-stream-registry nil)))
           (registry (cdr registry-entry)))
      ;; Add/update the stream in registry
      (setf (alist-get name registry nil nil #'equal) token)
      ;; Update the settings
      (setf (alist-get 'ob-tmux-asciinema-stream-registry nil-settings) registry)
      (setf (cdr nil-entry) nil-settings)
      ;; Update or add nil entry to existing
      (if (assq nil existing)
          (setf (cdr (assq nil existing)) nil-settings)
        (push nil-entry existing))
      ;; Write back with proper formatting
      (with-temp-file file
        (insert ";;; Directory Local Variables for personal settings\n")
        (insert ";;; Not committed to version control\n")
        (insert ";;; See Info node `(emacs) Directory Variables' for more information.\n\n")
        (pp existing (current-buffer)))
      (message "Added stream '%s' to %s" name file)
      ;; Invalidate buffer-local cache
      (setq ob-tmux-asciinema--buffer-registry-cache nil))))

(defun ob-tmux-asciinema--persist-stream-to-global (name token)
  "Add stream NAME -> TOKEN to global registry file."
  (let ((file (or ob-tmux-asciinema-stream-registry-file
                  (expand-file-name "~/.config/asciinema-streams.json"))))
    ;; Ensure directory exists
    (make-directory (file-name-directory file) t)
    ;; Load existing or create new
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (data (if (file-exists-p file)
                     (json-read-file file)
                   '((streams))))
           (streams (cdr (assq 'streams data))))
      ;; Add/update entry
      (setf (alist-get (intern name) streams) token)
      (setf (alist-get 'streams data) streams)
      ;; Write back
      (with-temp-file file
        (insert (json-encode data)))
      (message "Added stream '%s' to %s" name file)
      ;; Invalidate cache
      (setq ob-tmux-asciinema--stream-cache-mtime nil))))

(defun ob-tmux-asciinema--maybe-persist-stream (name token)
  "Possibly persist stream NAME -> TOKEN based on user preference."
  (pcase ob-tmux-asciinema-persist-auto-created-streams
    ('nil nil)  ; Don't persist
    ('file (ob-tmux-asciinema--persist-stream-to-file name token))
    ('dir-local (ob-tmux-asciinema--persist-stream-to-dir-local name token))
    ('global (ob-tmux-asciinema--persist-stream-to-global name token))
    ('prompt
     (let ((choice (read-char-choice
                    (format "Stream '%s' created. Persist? [f]ile, [d]ir-local, [g]lobal, [n]o: " name)
                    '(?f ?d ?g ?n))))
       (pcase choice
         (?f (ob-tmux-asciinema--persist-stream-to-file name token))
         (?d (ob-tmux-asciinema--persist-stream-to-dir-local name token))
         (?g (ob-tmux-asciinema--persist-stream-to-global name token))
         (?n nil))))))

(defun ob-tmux-asciinema--resolve-stream-from-api (name)
  "Resolve stream NAME via server API, creating if needed.
Returns plist with :ws-producer-url, :producer-token, :id, :url.
Returns nil if API unavailable or error."
  (condition-case err
      (let ((stream (ob-tmux-asciinema-api-get-or-create-stream name)))
        (when stream
          (let* ((ws-url (alist-get 'ws_producer_url stream))
                 (token (ob-tmux-asciinema--extract-producer-token ws-url))
                 (info (list :id (alist-get 'id stream)
                             :ws-producer-url ws-url
                             :producer-token token
                             :url (alist-get 'url stream))))
            ;; Cache for session
            (ob-tmux-asciinema--cache-stream name info)
            ;; Maybe persist
            (when token
              (ob-tmux-asciinema--maybe-persist-stream name token))
            info)))
    (error
     (message "Warning: Could not resolve stream '%s' from API: %s" name err)
     nil)))

(defun ob-tmux-asciinema-lookup-stream (name)
  "Lookup WebSocket producer URL for stream NAME.
NAME can be:
  - A friendly name (looked up in registry, then API)
  - A raw token (12+ alphanumeric chars, passed through)
  - A UUID (passed through directly)

Resolution order:
  1. Local registry (#+STREAM:, .dir-locals, customize)
  2. Session cache (previously resolved this session)
  3. Server API (find by title or create new)
  4. Direct use (if NAME looks like a raw token)

Returns the WebSocket producer URL string."
  ;; If it looks like a raw token, use it directly
  (if (ob-tmux-asciinema--looks-like-token-p name)
      (ob-tmux-asciinema--build-websocket-url name nil)
    ;; Try registry first
    (let ((registry (ob-tmux-asciinema--get-combined-registry)))
      (if-let ((token (cdr (assoc name registry))))
          ;; Found in registry
          (ob-tmux-asciinema--build-websocket-url token nil)
        ;; Check session cache
        (if-let ((cached (ob-tmux-asciinema--get-cached-stream name)))
            (plist-get cached :ws-producer-url)
          ;; Try API if enabled
          (if ob-tmux-asciinema-auto-create-streams
              (if-let ((resolved (ob-tmux-asciinema--resolve-stream-from-api name)))
                  (plist-get resolved :ws-producer-url)
                ;; API failed - error
                (error "Stream '%s' not found and could not be created. Check server connection." name))
            ;; Auto-create disabled - error with helpful message
            (error "Stream '%s' not found. Define with #+STREAM: %s TOKEN, or enable `ob-tmux-asciinema-auto-create-streams'."
                   name name)))))))

(defun ob-tmux-asciinema-list-streams ()
  "List all registered streams from all configuration levels."
  (interactive)
  (ob-tmux-asciinema--load-registry-file)  ; Ensure global file is loaded
  (let ((registry (ob-tmux-asciinema--get-combined-registry))
        (session-cache ob-tmux-asciinema--session-stream-cache))
    (if (or registry session-cache)
        (with-output-to-temp-buffer "*Asciinema Streams*"
          (when registry
            (princ "=== Persisted Streams ===\n")
            (princ "(from #+STREAM:, .dir-locals, or global registry)\n\n")
            (princ (format "%-20s %s\n" "NAME" "TOKEN (truncated)"))
            (princ (make-string 50 ?-))
            (princ "\n")
            (dolist (pair registry)
              (princ (format "%-20s %s...\n"
                             (car pair)
                             (substring (cdr pair) 0 (min 12 (length (cdr pair)))))))
            (princ "\n"))
          (when session-cache
            (princ "=== Session Cache ===\n")
            (princ "(auto-created this session, not yet persisted)\n\n")
            (princ (format "%-20s %-8s %s\n" "NAME" "ID" "URL"))
            (princ (make-string 60 ?-))
            (princ "\n")
            (dolist (pair session-cache)
              (let ((name (car pair))
                    (info (cdr pair)))
                (princ (format "%-20s %-8s %s\n"
                               name
                               (or (plist-get info :id) "?")
                               (or (plist-get info :url) "?")))))))
      (message "No streams registered. Use `:stream-to name` to auto-create, or set `ob-tmux-asciinema-stream-registry'."))))

(defun ob-tmux-asciinema-clear-session-cache ()
  "Clear the session stream cache."
  (interactive)
  (setq ob-tmux-asciinema--session-stream-cache nil)
  (message "Session stream cache cleared."))

;;; Metadata extraction

(defun ob-tmux-asciinema--get-org-title ()
  "Get the #+TITLE from current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TITLE:[ \t]*\\(.+\\)$" nil t)
      (string-trim (match-string 1)))))

(defun ob-tmux-asciinema--get-org-filetags ()
  "Get #+FILETAGS from current buffer as a list."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+FILETAGS:[ \t]*\\(.+\\)$" nil t)
      (let ((tags-str (match-string 1)))
        ;; Parse :tag1:tag2:tag3: format
        (split-string tags-str ":" t "[ \t]+")))))

(defun ob-tmux-asciinema--get-heading-title ()
  "Get the current org heading title."
  (save-excursion
    (when (org-before-first-heading-p)
      (org-back-to-heading-or-point-min t))
    (when (org-at-heading-p)
      (org-get-heading t t t t))))

(defun ob-tmux-asciinema--get-block-name ()
  "Get the #+NAME of the current src block."
  (save-excursion
    (when (org-in-src-block-p)
      (goto-char (org-element-property :begin (org-element-at-point)))
      (forward-line -1)
      (when (looking-at "^#\\+NAME:[ \t]*\\(.+\\)$")
        (string-trim (match-string 1))))))

(defun ob-tmux-asciinema--get-file-property (property)
  "Get a file-level #+PROPERTY value."
  (save-excursion
    (goto-char (point-min))
    (let ((prop-re (format "^#\\+PROPERTY:[ \t]+%s[ \t]+\\(.+\\)$" property)))
      (when (re-search-forward prop-re nil t)
        (string-trim (match-string 1))))))

(defun ob-tmux-asciinema--parse-session (session)
  "Parse SESSION string into (session-name . window-name).
Handles formats: \"session\", \"session:window\", \"session:window.pane\"."
  (when session
    (let* ((parts (split-string session "[:.]+"))
           (session-name (car parts))
           (window-name (cadr parts)))
      (cons session-name window-name))))

(defun ob-tmux-asciinema--build-title (params)
  "Build a title from PARAMS and org context.
Priority: :title header > block name > heading > #+TITLE."
  (or (cdr (assq :title params))
      (let ((block-name (ob-tmux-asciinema--get-block-name))
            (heading (ob-tmux-asciinema--get-heading-title))
            (org-title (ob-tmux-asciinema--get-org-title)))
        (cond
         ;; Block name + context
         ((and block-name org-title)
          (format "%s - %s" org-title block-name))
         (block-name block-name)
         ;; Heading + context
         ((and heading org-title)
          (format "%s - %s" org-title heading))
         (heading heading)
         ;; Just org title
         (org-title org-title)
         ;; Fallback
         (t "ob-tmux capture")))))

(defun ob-tmux-asciinema--build-tags (params)
  "Build a list of tags from PARAMS and org context."
  (let ((tags '()))
    ;; Explicit tags from header
    (when-let ((explicit-tags (cdr (assq :tags params))))
      (dolist (tag (split-string explicit-tags "[,; ]+" t))
        (push tag tags)))
    ;; File tags
    (dolist (tag (ob-tmux-asciinema--get-org-filetags))
      (push tag tags))
    ;; Auto-generated tags
    (when ob-tmux-asciinema-auto-tags
      ;; File name tag
      (when buffer-file-name
        (push (format "file:%s" (file-name-nondirectory buffer-file-name)) tags))
      ;; Session/window tags
      (when-let ((session (cdr (assq :session params))))
        (let ((parsed (ob-tmux-asciinema--parse-session session)))
          (when (car parsed)
            (push (format "session:%s" (car parsed)) tags))
          (when (cdr parsed)
            (push (format "window:%s" (cdr parsed)) tags)))))
    ;; Remove duplicates and return
    (delete-dups (nreverse tags))))

(defun ob-tmux-asciinema--build-cast-path (params)
  "Build the cast file path from PARAMS and context."
  (let* ((cast-dir (or (cdr (assq :cast-dir params))
                       (ob-tmux-asciinema--get-file-property "cast-dir")
                       ob-tmux-asciinema-cast-dir))
         (base-name (or (ob-tmux-asciinema--get-block-name)
                        (format-time-string "capture-%Y%m%d-%H%M%S")))
         (file-name (format "%s.cast" base-name)))
    ;; Ensure directory exists
    (unless (file-directory-p cast-dir)
      (make-directory cast-dir t))
    (expand-file-name file-name cast-dir)))

;;; Command building

(defun ob-tmux-asciinema--build-record-args (body params cast-file)
  "Build asciinema record command arguments for BODY with PARAMS saving to CAST-FILE."
  (let ((args (list "rec" "--quiet" "--overwrite")))
    ;; Output format (always v3)
    (push "--output-format" args)
    (push "asciicast-v3" args)
    ;; Title
    (when-let ((title (ob-tmux-asciinema--build-title params)))
      (push "--title" args)
      (push title args))
    ;; Idle time limit
    (let ((idle-limit (or (cdr (assq :idle-limit params))
                          ob-tmux-asciinema-idle-time-limit)))
      (when idle-limit
        (push "--idle-time-limit" args)
        (push (format "%s" idle-limit) args)))
    ;; Capture input
    (when (or (cdr (assq :capture-input params))
              ob-tmux-asciinema-capture-input)
      (push "--capture-input" args))
    ;; Window size (from tmux if available)
    ;; Command to execute
    (push "--command" args)
    (push (format "bash -c %s" (shell-quote-argument body)) args)
    ;; Output file
    (push cast-file args)
    (nreverse args)))

(defun ob-tmux-asciinema--build-websocket-url (producer-token params)
  "Build WebSocket URL for streaming with PRODUCER-TOKEN.
Uses server URL from PARAMS, file property, or customization."
  (let* ((server-url (or (cdr (assq :server-url params))
                         (ob-tmux-asciinema--get-file-property "asciinema-server")
                         ob-tmux-asciinema-server-url
                         (getenv "ASCIINEMA_SERVER_URL")))
         (ws-scheme (if (and server-url (string-prefix-p "https" server-url))
                        "wss" "ws"))
         (host (when server-url
                 (replace-regexp-in-string "^https?://" "" server-url))))
    (if host
        (format "%s://%s/ws/S/%s" ws-scheme host producer-token)
      ;; No server configured - just use token directly (CLI will handle it)
      producer-token)))

(defun ob-tmux-asciinema--build-stream-args (body params cast-file)
  "Build asciinema session (record+stream) command arguments."
  (let ((args (list "session" "--quiet")))
    ;; Output file
    (push "--output-file" args)
    (push cast-file args)
    ;; Output format
    (push "--output-format" args)
    (push "asciicast-v3" args)
    ;; Remote streaming - resolve name through registry and build WebSocket URL
    (when-let ((stream-to (or (cdr (assq :stream-to params))
                              ob-tmux-asciinema-default-stream-id)))
      (let* ((token (ob-tmux-asciinema-lookup-stream stream-to))
             (ws-url (ob-tmux-asciinema--build-websocket-url token params)))
        (push "--stream-remote" args)
        (push ws-url args)))
    ;; Local streaming
    (when-let ((stream-local (cdr (assq :stream-local params))))
      (push "--stream-local" args)
      (when (not (eq stream-local t))
        (push (format "0.0.0.0:%s" stream-local) args)))
    ;; Title
    (when-let ((title (ob-tmux-asciinema--build-title params)))
      (push "--title" args)
      (push title args))
    ;; Tags
    (when-let ((tags (ob-tmux-asciinema--build-tags params)))
      (push "--tags" args)
      (push (mapconcat #'identity tags ",") args))
    ;; Idle time limit
    (let ((idle-limit (or (cdr (assq :idle-limit params))
                          ob-tmux-asciinema-idle-time-limit)))
      (when idle-limit
        (push "--idle-time-limit" args)
        (push (format "%s" idle-limit) args)))
    ;; Capture input
    (when (or (cdr (assq :capture-input params))
              ob-tmux-asciinema-capture-input)
      (push "--capture-input" args))
    ;; Command to execute
    (push "--command" args)
    (push (format "bash -c %s" (shell-quote-argument body)) args)
    (nreverse args)))

(defun ob-tmux-asciinema--build-command (body params)
  "Build the full asciinema command for BODY with PARAMS.
Returns a plist with :command, :cast-file, and :streaming."
  (let* ((cast-file (ob-tmux-asciinema--build-cast-path params))
         (stream-to (cdr (assq :stream-to params)))
         (stream-local (cdr (assq :stream-local params)))
         (streaming (or stream-to stream-local))
         (args (if streaming
                   (ob-tmux-asciinema--build-stream-args body params cast-file)
                 (ob-tmux-asciinema--build-record-args body params cast-file)))
         (command (mapconcat #'shell-quote-argument
                             (cons ob-tmux-asciinema-binary args) " ")))
    (list :command command
          :cast-file cast-file
          :streaming streaming
          :title (ob-tmux-asciinema--build-title params)
          :tags (ob-tmux-asciinema--build-tags params))))

;;; Property management

(defun ob-tmux-asciinema--insert-property (name value)
  "Insert a #+NAME: VALUE property after the current src block end."
  (save-excursion
    (when (org-in-src-block-p)
      (goto-char (org-element-property :end (org-element-at-point)))
      (skip-chars-backward " \t\n")
      (forward-line 1)
      ;; Check if property already exists
      (let ((prop-re (format "^#\\+%s:" name)))
        (if (looking-at prop-re)
            ;; Update existing
            (progn
              (delete-region (point) (line-end-position))
              (insert (format "#+%s: %s" name value)))
          ;; Insert new
          (insert (format "#+%s: %s\n" name value)))))))

(defun ob-tmux-asciinema--update-block-properties (cast-file &optional stream-url duration)
  "Update properties after execution with CAST-FILE, STREAM-URL, and DURATION."
  (when cast-file
    (ob-tmux-asciinema--insert-property "CAST" cast-file))
  (when stream-url
    (ob-tmux-asciinema--insert-property "STREAM_URL" stream-url))
  (when duration
    (ob-tmux-asciinema--insert-property "DURATION" (format "%.1f" duration))))

;;; Interactive commands

(defun ob-tmux-asciinema-play-at-point ()
  "Play the cast file associated with the current block."
  (interactive)
  (save-excursion
    (when (org-in-src-block-p)
      (goto-char (org-element-property :end (org-element-at-point)))
      (when (re-search-forward "^#\\+CAST:[ \t]*\\(.+\\)$" nil t)
        (let ((cast-file (string-trim (match-string 1))))
          (if (file-exists-p cast-file)
              (start-process "asciinema-play" nil
                             ob-tmux-asciinema-binary "play" cast-file)
            (message "Cast file not found: %s" cast-file)))))))

(defun ob-tmux-asciinema-open-in-browser ()
  "Open the stream URL associated with the current block in a browser."
  (interactive)
  (save-excursion
    (when (org-in-src-block-p)
      (goto-char (org-element-property :end (org-element-at-point)))
      (when (re-search-forward "^#\\+STREAM_URL:[ \t]*\\(.+\\)$" nil t)
        (browse-url (string-trim (match-string 1)))))))

(defun ob-tmux-asciinema-get-cast-info ()
  "Get information about the cast file at point."
  (interactive)
  (save-excursion
    (when (org-in-src-block-p)
      (goto-char (org-element-property :end (org-element-at-point)))
      (when (re-search-forward "^#\\+CAST:[ \t]*\\(.+\\)$" nil t)
        (let ((cast-file (string-trim (match-string 1))))
          (if (file-exists-p cast-file)
              (let* ((attrs (file-attributes cast-file))
                     (size (file-attribute-size attrs))
                     (mtime (file-attribute-modification-time attrs)))
                (message "Cast: %s\nSize: %s bytes\nModified: %s"
                         cast-file size (format-time-string "%Y-%m-%d %H:%M:%S" mtime)))
            (message "Cast file not found: %s" cast-file)))))))

;;; CAST_LOG drawer - recording history
;;
;; The CAST_LOG drawer stores a history of recordings made from a source block.
;; It lives right after the #+RESULTS: block and uses org list format with links.
;;
;; Format:
;;   #+RESULTS: block-name
;;   : command output here
;;   :CAST_LOG:
;;   - [2024-01-15 Mon 12:30] [[https://...com/a/xyz][cast]] | [[file:~/casts/demo.cast][local]] | 5.2s
;;   - [2024-01-14 Sun 10:15] [[https://...com/a/abc][cast]] | 3.1s
;;   :END:

(defcustom ob-tmux-asciinema-cast-log-enabled nil
  "Whether to maintain CAST_LOG drawer for recording history.
When nil (default), no drawer is created.
Can be overridden per-block with `:log yes' or `:log no' header arg."
  :type 'boolean
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-cast-log-max-entries 10
  "Maximum number of entries to keep in CAST_LOG drawer.
Set to nil for unlimited history."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'ob-tmux-asciinema)

(defcustom ob-tmux-asciinema-cast-log-include-local t
  "Whether to include local file path in CAST_LOG entries."
  :type 'boolean
  :group 'ob-tmux-asciinema)

(defun ob-tmux-asciinema--find-results-end ()
  "Find the end position of #+RESULTS: block for current src block.
Returns point at end of results, or nil if no results."
  (save-excursion
    (when (org-in-src-block-p)
      (let* ((src-block (org-element-at-point))
             (src-end (org-element-property :end src-block))
             (block-name (org-element-property :name src-block)))
        (goto-char src-end)
        (skip-chars-forward " \t\n")
        ;; Look for #+RESULTS:
        (when (looking-at (if block-name
                              (format "^#\\+RESULTS:\\(?: %s\\)?[ \t]*$" (regexp-quote block-name))
                            "^#\\+RESULTS:[ \t]*$"))
          (forward-line 1)
          ;; Skip over results content (: prefixed lines, drawer, table, etc.)
          (while (and (not (eobp))
                      (or (looking-at "^[ \t]*:")  ; fixed-width or drawer
                          (looking-at "^[ \t]*|")  ; table
                          (looking-at "^[ \t]*#\\+") ; affiliated keywords
                          (looking-at "^[ \t]*$"))) ; blank lines within
            (forward-line 1))
          ;; Back up over trailing blank lines
          (skip-chars-backward " \t\n")
          (end-of-line)
          (point))))))

(defun ob-tmux-asciinema--find-cast-log-drawer ()
  "Find existing CAST_LOG drawer after current src block's results.
Returns (START . END) of drawer content, or nil if not found."
  (save-excursion
    (when-let ((results-end (ob-tmux-asciinema--find-results-end)))
      (goto-char results-end)
      (forward-line 1)
      (skip-chars-forward " \t\n")
      (when (looking-at "^:CAST_LOG:[ \t]*$")
        (let ((start (progn (forward-line 1) (point))))
          (when (re-search-forward "^:END:[ \t]*$" nil t)
            (cons start (match-beginning 0))))))))

(defun ob-tmux-asciinema--format-cast-log-entry (info)
  "Format a CAST_LOG entry from INFO plist.
INFO should contain :cast-url, :stream-url, :cast-file, :duration, :timestamp."
  (let* ((timestamp (or (plist-get info :timestamp)
                        (format-time-string "[%Y-%m-%d %a %H:%M]")))
         (cast-url (plist-get info :cast-url))
         (stream-url (plist-get info :stream-url))
         (cast-file (plist-get info :cast-file))
         (duration (plist-get info :duration))
         (parts (list (format "- %s" timestamp))))
    ;; Cast URL (remote recording)
    (when cast-url
      (push (format "[[%s][cast]]" cast-url) parts))
    ;; Stream URL
    (when stream-url
      (push (format "[[%s][stream]]" stream-url) parts))
    ;; Local file
    (when (and ob-tmux-asciinema-cast-log-include-local cast-file)
      (push (format "[[file:%s][local]]" cast-file) parts))
    ;; Duration (can be number or string)
    (when duration
      (push (if (numberp duration)
                (format "%.1fs" duration)
              (format "%s" duration))
            parts))
    ;; Join with separators (timestamp first, then | separated)
    (let ((timestamp-part (car (last parts)))
          (rest (butlast parts)))
      (concat timestamp-part " " (mapconcat #'identity (nreverse rest) " | ")))))

(defun ob-tmux-asciinema--parse-cast-log-entries (content)
  "Parse CAST_LOG drawer CONTENT into list of entry strings."
  (when content
    (split-string content "\n" t "[ \t]+")))

(defun ob-tmux-asciinema-add-cast-log-entry (info &optional params)
  "Add a new entry to the CAST_LOG drawer for current src block.
INFO is a plist with :cast-url, :stream-url, :cast-file, :duration.
PARAMS is the org-babel params alist for checking :log header arg.
Creates the drawer if it doesn't exist.
Respects `ob-tmux-asciinema-cast-log-enabled' and :log header arg."
  (let* ((log-arg (cdr (assq :log params)))
         (enabled (cond
                   ((member log-arg '("yes" "t" t)) t)
                   ((member log-arg '("no" "nil" nil))
                    (if log-arg nil ob-tmux-asciinema-cast-log-enabled))
                   (t ob-tmux-asciinema-cast-log-enabled))))
    (when enabled
      (save-excursion
        (let ((entry (ob-tmux-asciinema--format-cast-log-entry info))
              (existing-drawer (ob-tmux-asciinema--find-cast-log-drawer)))
          (if existing-drawer
          ;; Update existing drawer
          (let* ((start (car existing-drawer))
                 (end (cdr existing-drawer))
                 (content (buffer-substring-no-properties start end))
                 (entries (ob-tmux-asciinema--parse-cast-log-entries content))
                 (new-entries (cons entry entries)))
            ;; Limit entries if configured
            (when (and ob-tmux-asciinema-cast-log-max-entries
                       (> (length new-entries) ob-tmux-asciinema-cast-log-max-entries))
              (setq new-entries (cl-subseq new-entries 0 ob-tmux-asciinema-cast-log-max-entries)))
            ;; Replace drawer content
            (goto-char start)
            (delete-region start end)
            (insert (mapconcat #'identity new-entries "\n") "\n"))
        ;; Create new drawer after results
        (when-let ((results-end (ob-tmux-asciinema--find-results-end)))
          (goto-char results-end)
          (end-of-line)
          (insert "\n:CAST_LOG:\n" entry "\n:END:"))))))))

(defun ob-tmux-asciinema-get-latest-cast-log-entry ()
  "Get the most recent CAST_LOG entry for current src block.
Returns the entry string or nil."
  (when-let ((drawer (ob-tmux-asciinema--find-cast-log-drawer)))
    (let* ((content (buffer-substring-no-properties (car drawer) (cdr drawer)))
           (entries (ob-tmux-asciinema--parse-cast-log-entries content)))
      (car entries))))

(defun ob-tmux-asciinema-browse-latest-cast ()
  "Open the most recent cast URL from CAST_LOG in browser."
  (interactive)
  (when-let ((entry (ob-tmux-asciinema-get-latest-cast-log-entry)))
    (when (string-match "\\[\\[\\(https?://[^]]+\\)\\]\\[cast\\]\\]" entry)
      (browse-url (match-string 1 entry)))))

(defun ob-tmux-asciinema-play-latest-local-cast ()
  "Play the most recent local cast file from CAST_LOG."
  (interactive)
  (when-let ((entry (ob-tmux-asciinema-get-latest-cast-log-entry)))
    (when (string-match "\\[\\[file:\\([^]]+\\)\\]\\[local\\]\\]" entry)
      (let ((file (match-string 1 entry)))
        (if (file-exists-p file)
            (start-process "asciinema-play" nil ob-tmux-asciinema-binary "play" file)
          (message "Local cast file not found: %s" file))))))

(defun ob-tmux-asciinema-list-cast-log ()
  "Display all CAST_LOG entries for current src block."
  (interactive)
  (if-let ((drawer (ob-tmux-asciinema--find-cast-log-drawer)))
      (let* ((content (buffer-substring-no-properties (car drawer) (cdr drawer)))
             (entries (ob-tmux-asciinema--parse-cast-log-entries content)))
        (with-output-to-temp-buffer "*Cast Log*"
          (princ "Cast recording history:\n\n")
          (dolist (entry entries)
            (princ entry)
            (princ "\n"))))
    (message "No CAST_LOG found for this block")))

;;; Pane Stream Setup Commands

(defun ob-tmux-asciinema-setup-pane-stream (session &optional stream-name)
  "Set up a tmux SESSION with asciinema stream recording.
If STREAM-NAME is not provided, derives it from SESSION.
Creates the stream on the server if needed, then creates the tmux
pane wrapped with asciinema session.

Interactively, prompts for session and optional stream name."
  (interactive
   (list (read-string "Session (e.g., demo:main): ")
         (let ((name (read-string "Stream name (leave empty to auto-derive): ")))
           (unless (string-empty-p name) name))))
  (unless ob-tmux-asciinema-server-url
    (user-error "Server URL not configured. Set ob-tmux-asciinema-server-url"))
  (let* ((stream-name (or stream-name
                          (ob-tmux-capture--derive-stream-name session)))
         ;; Check if stream exists or create it
         (registry (ob-tmux-asciinema--get-combined-registry))
         (existing-token (cdr (assoc stream-name registry)))
         stream-info)
    ;; Get or create stream
    (if existing-token
        ;; Use existing stream
        (let ((server-info (ob-tmux-asciinema-api-get-stream existing-token)))
          (if server-info
              (setq stream-info
                    (list (cons :id (or (alist-get 'id server-info) existing-token))
                          (cons :url (alist-get 'url server-info))
                          (cons :token existing-token)))
            (user-error "Stream %s not found on server" stream-name)))
      ;; Try to find or create stream by name
      (let ((streams (ob-tmux-asciinema-api-list-streams stream-name)))
        (if-let ((match (seq-find (lambda (s) (equal (alist-get 'id s) stream-name))
                                  streams)))
            (setq stream-info
                  (list (cons :id (alist-get 'id match))
                        (cons :url (alist-get 'url match))
                        (cons :token (alist-get 'producer_token match))))
          ;; TODO: Could create stream via API if server supports it
          (user-error "Stream %s not found. Create it on the server first" stream-name))))
    ;; Set up the pane
    (require 'ob-tmux-capture)
    (let ((result (ob-tmux-capture-setup-pane-stream session nil stream-info)))
      (when result
        ;; Add to org file
        (ob-tmux-asciinema-add-pane-stream-keyword
         session stream-name (alist-get :url stream-info))
        (message "Pane %s now streaming to %s" session (alist-get :url stream-info))
        result))))

(defun ob-tmux-asciinema-list-pane-streams ()
  "List all pane → stream mappings in current file."
  (interactive)
  (let ((mappings (ob-tmux-asciinema--get-file-pane-streams)))
    (if mappings
        (with-output-to-temp-buffer "*Pane Streams*"
          (princ "Pane → Stream mappings:\n\n")
          (dolist (mapping mappings)
            (let ((pane (car mapping))
                  (stream-name (cadr mapping))
                  (stream-url (cddr mapping)))
              (princ (format "  %s → %s" pane stream-name))
              (when stream-url
                (princ (format " (%s)" stream-url)))
              (princ "\n"))))
      (message "No pane streams configured in this file"))))

(defun ob-tmux-asciinema-sync-pane-streams ()
  "Sync pane stream mappings from tmux to org file.
Reads stream info from tmux environment and updates org file keywords."
  (interactive)
  (let ((updated 0))
    ;; Get all tmux sessions
    (let* ((cmd "tmux list-sessions -F '#{session_name}'")
           (sessions (split-string (string-trim (shell-command-to-string cmd)) "\n" t)))
      (dolist (session sessions)
        ;; Get windows in session
        (let* ((cmd (format "tmux list-windows -t %s -F '#{window_name}'"
                            (shell-quote-argument session)))
               (windows (split-string (string-trim (shell-command-to-string cmd)) "\n" t)))
          (dolist (window windows)
            (let* ((pane (format "%s:%s" session window))
                   (stream-info (ob-tmux-capture-get-pane-stream pane nil)))
              (when stream-info
                (ob-tmux-asciinema-add-pane-stream-keyword
                 pane
                 (alist-get :id stream-info)
                 (alist-get :url stream-info))
                (cl-incf updated)))))))
    (if (> updated 0)
        (message "Synced %d pane stream mapping(s) to org file" updated)
      (message "No pane streams found in tmux sessions"))))

;;; Integration with ob-tmux-capture

(defun ob-tmux-asciinema-execute (body params)
  "Execute BODY with asciinema recording/streaming based on PARAMS.
This is called from ob-tmux-capture when :capture is 'asciinema'."
  (let* ((cmd-info (ob-tmux-asciinema--build-command body params))
         (command (plist-get cmd-info :command))
         (cast-file (plist-get cmd-info :cast-file))
         (streaming (plist-get cmd-info :streaming)))
    (message "Asciinema: %s" (if streaming "streaming + recording" "recording"))
    (message "Cast file: %s" cast-file)
    (message "Command: %s" command)
    ;; Return command info for caller to execute
    cmd-info))

(defun ob-tmux-asciinema-record-execution (info &optional params)
  "Record an execution in the CAST_LOG drawer.
INFO is a plist with :cast-url :stream-url :cast-file :duration.
PARAMS is the org-babel params alist for :log header arg.
Call this after asciinema execution completes."
  (when (or (plist-get info :cast-url)
            (plist-get info :cast-file))
    (ob-tmux-asciinema-add-cast-log-entry info params)))

(provide 'ob-tmux-asciinema)
;;; ob-tmux-asciinema.el ends here
