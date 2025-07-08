(require 'json-rpc-server)
(require 'json)

(defvar jsonrpc-server-process nil)
(defvar mcp-server-initialized nil
  "Whether the MCP server has been initialized.")
(defvar mcp-client-ready nil
  "Whether the client has sent initialized notification.")

;; MCP initialization method
(defun initialize (&optional protocol-version capabilities client-info)
  "Handle MCP initialization request."
  ;; The json-rpc-server library passes params as individual arguments
  ;; protocol-version comes as ("protocolVersion" . "2024-11-05")
  (let ((actual-protocol-version 
         (if (consp protocol-version)
             (cdr protocol-version)  ; Extract the value from the cons cell
           protocol-version)))
    
    (unless actual-protocol-version
      (error "Protocol version is required"))
    
    ;; Validate protocol version
    (unless (string= actual-protocol-version "2025-06-18")
      (error "Unsupported protocol version: %s. Server supports: 2025-06-18" actual-protocol-version))
    
    (setq mcp-server-initialized t)
    
    ;; Return server capabilities and info
    `((protocolVersion . "2025-06-18")
      (capabilities . ((logging . ())
                      ;; (prompts . ((listChanged . t)))
                      ;; (resources . ((subscribe . t)
                      ;;               (listChanged . t)))
                      (tools . ((listChanged . t)
                                ))))
      (serverInfo . ((name . "emacs-mcp-server")
                    (title . "Emacs MCP Server")
                    (version . "1.0.0")))
      (instructions . "This is an Emacs-based MCP server for demonstration purposes."))))

;; Handle initialized notification
(defun notifications/initialized ()
  "Handle initialized notification from client."
  (setq mcp-client-ready t)
  (message "Client is ready for normal operations")
  ;; Notifications don't return a response
  nil)

;; Tools list method
(defun tools/list (&optional cursor)
  "Handle tools/list method calls."
  (unless mcp-client-ready
    (error "Client not ready. Send initialized notification first."))
  `((tools . (((name . "list-buffers")
               (title . "List Emacs buffers")
               (description . "List all open Emacs buffers")
               (inputSchema . ((type . "object")
                               (properties . ())
                               (required . ("")))))
              ((name . "read-buffer")
               (title . "Read buffer content")
               (description . "Read the content of a specific Emacs buffer by name")
               (inputSchema . ((type . "object")
                               (properties . ((buffer-name . ((type . "string")
                                                               (description . "Name of the buffer to read")))))
                               (required . ("buffer-name")))))
              ((name . "edit-buffer")
               (title . "Edit buffer content")
               (description . "Replace the content of a specific Emacs buffer")
               (inputSchema . ((type . "object")
                               (properties . ((buffer-name . ((type . "string")
                                                               (description . "Name of the buffer to edit")))
                                              (content . ((type . "string")
                                                          (description . "New content for the buffer")))))
                               (required . ("buffer-name" "content")))))))))

;; ;; Resources list method
;; (defun resources/list (&optional cursor)
;;   "Handle resources/list method calls."
;;   (unless mcp-client-ready
;;     (error "Client not ready. Send initialized notification first."))
;;   `((resources . (((uri . "file://example.txt")
;;                    (name . "example.txt")
;;                    (description . "Example text file")
;;                    (mimeType . "text/plain"))
;;                   ((uri . "file://config.json")
;;                    (name . "config.json")
;;                    (description . "Configuration file")
;;                    (mimeType . "application/json"))))))

;; List buffers tool/method
(defun mcp/list-buffers ()
  "Handle mcp/list-buffers method calls."
  (unless mcp-client-ready
    (error "Client not ready. Send initialized notification first."))
  (let ((buffer-list (mapcar (lambda (buf) (buffer-name buf)) (buffer-list))))
    (format "Open Emacs buffers:\n%s" (mapconcat 'identity buffer-list "\n"))))

;; Read buffer tool/method
(defun mcp/read-buffer (buffer-name)
  "Handle mcp/read-buffer method calls."
  (unless mcp-client-ready
    (error "Client not ready. Send initialized notification first."))
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
        (with-current-buffer buffer (buffer-string))
      (error "Buffer '%s' not found" buffer-name))))

;; Edit buffer tool/method
(defun mcp/edit-buffer (buffer-name content)
  "Handle mcp/edit-buffer method calls."
  (unless mcp-client-ready
    (error "Client not ready. Send initialized notification first."))
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
        (progn
          ;; Create backup for diff
          (let ((backup-name (concat buffer-name "-backup")))
            (when (get-buffer backup-name) (kill-buffer backup-name))
            (with-current-buffer buffer (clone-buffer backup-name)))
          
          ;; Create temporary buffer with new content
          (let ((temp-name (concat buffer-name "-temp")))
            (when (get-buffer temp-name)
              (kill-buffer temp-name))
            (with-current-buffer (get-buffer-create temp-name)
              (insert content))
            
            ;; Show diff between original and proposed changes
            (diff-buffers (get-buffer (concat buffer-name "-backup")) (get-buffer temp-name))
            
            ;; Ask user to accept or deny
            (if (y-or-n-p "Accept these changes? ")
                (progn
                  ;; Apply changes
                  (with-current-buffer buffer (erase-buffer) (insert content))
                  (kill-buffer temp-name)
                  (doom-kill-buffer-and-windows "*Diff*")
                  (format "Buffer '%s' has been edited successfully" buffer-name))
              (progn
                ;; Reject changes
                (kill-buffer temp-name)
                (doom-kill-buffer-and-windows "*Diff*")
                (format "Changes to buffer '%s' were rejected" buffer-name)))))
      (error "Buffer '%s' not found" buffer-name))))

;; Tools call method  
(defun tools/call (&optional name arguments)
  "Handle tools/call method calls."
  (unless mcp-client-ready
    (error "Client not ready. Send initialized notification first."))
  ;; (debug)
  (let ((actual-name (cdr name))
        (actual-arguments (cdr arguments)))
    (cond
     ((string= actual-name "list-buffers")
      `((content . (((type . "text")
                     (text . ,(mcp/list-buffers)))))))
     ((string= actual-name "read-buffer")
      (let ((buffer-name (cdr (assoc "buffer-name" actual-arguments))))
        (unless buffer-name
          (error "buffer-name argument is required"))
        `((content . (((type . "text")
                       (text . ,(mcp/read-buffer buffer-name))))))))
     ((string= actual-name "edit-buffer")
      (let ((buffer-name (cdr (assoc "buffer-name" actual-arguments)))
            (content (cdr (assoc "content" actual-arguments))))
        (unless buffer-name
          (error "buffer-name argument is required"))
        (unless content
          (error "content argument is required"))
        `((content . (((type . "text")
                       (text . ,(mcp/edit-buffer buffer-name content))))))))
     (t (error "Unknown tool: %s" actual-name)))))

;; Method to check if server is initialized
(defun mcp-require-initialization ()
  "Check if server is initialized, return error if not."
  (unless mcp-server-initialized
    (error "Server not initialized. Call initialize first.")))

(defun jsonrpc-server-handler (proc string)
  "Handle HTTP requests for JSON-RPC."
  (let* ((lines (split-string string "\r\n"))
         (request-line (car lines))
         (request-parts (split-string request-line " "))
         (path (nth 1 request-parts))
         (headers (cdr lines))
         (body-start (cl-position "" headers :test 'string=))
         (body (if body-start
                   (mapconcat 'identity 
                             (nthcdr (1+ body-start) headers) 
                             "\r\n")
                 "")))
    
    ;; Check if request is for /mcp route
    (if (not (string= path "/mcp"))
        ;; Return 404 for non-/mcp routes
        (process-send-string proc "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n")
      
      ;; Process /mcp route
      (when (> (length body) 0)
        (let* ((request (json-parse-string body :object-type 'alist))
               (method (alist-get 'method request))
               (id (alist-get 'id request))
               (params (alist-get 'params request)))
          
          ;; Log request information
          (message "Received request - Method: %s, ID: %s, Params: %s" 
                   method id params)
          
          ;; Handle notifications separately (no ID)
          (if (null id)
              (progn
                ;; Process notification
                (cond
                 ((string= method "notifications/initialized")
                  (notifications/initialized))
                 (t
                  (message "Unknown notification: %s" method)))
                ;; Send empty response for notifications
                (process-send-string proc "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"))
            
            ;; Handle regular requests with json-rpc-server
            (let* ((response-json (json-rpc-server-handle body '(initialize mcp/list-buffers mcp/read-buffer tools/list tools/call)))
                   (response-length (length response-json)))
              (progn
                (message response-json)
                (process-send-string proc
                                     (format "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s"
                                             response-length
                                             response-json))))))))
  (process-send-eof proc)))

(defun jsonrpc-start-server-daemon ()
  "Start JSON-RPC server daemon on PORT."
  ;; (interactive "nPort: ")
  (interactive)
  (when jsonrpc-server-process
    (delete-process jsonrpc-server-process))
  (setq jsonrpc-server-process
        (make-network-process
         :name "jsonrpc-server"
         :service 3000
         :server t
         :host 'local
         :family 'ipv4
         :filter 'jsonrpc-server-handler))
  (message "JSON-RPC server started on port %d" 3000))

(defun jsonrpc-stop-server-daemon ()
  "Stop the JSON-RPC server daemon."
  (interactive)
  (if jsonrpc-server-process
      (progn
        (delete-process jsonrpc-server-process)
        (setq jsonrpc-server-process nil)
        (message "JSON-RPC server stopped"))
    (message "No JSON-RPC server is running")))
