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
                      (prompts . ((listChanged . t)))
                      (resources . ((subscribe . t)
                                    (listChanged . t)))
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
                               (required . ("something")))))))))

;; Resources list method
(defun resources/list (&optional cursor)
  "Handle resources/list method calls."
  (unless mcp-client-ready
    (error "Client not ready. Send initialized notification first."))
  `((resources . (((uri . "file://example.txt")
                   (name . "example.txt")
                   (description . "Example text file")
                   (mimeType . "text/plain"))
                  ((uri . "file://config.json")
                   (name . "config.json")
                   (description . "Configuration file")
                   (mimeType . "application/json"))))))

;; List buffers tool/method
(defun list-buffers ()
  "Handle list-buffers method calls."
  (unless mcp-client-ready
    (error "Client not ready. Send initialized notification first."))
  (let ((buffer-list (mapcar (lambda (buf) (buffer-name buf)) (buffer-list))))
    (format "Open Emacs buffers:\n%s" (mapconcat 'identity buffer-list "\n"))))

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
            (let* ((response-json (json-rpc-server-handle body '(initialize list-buffers tools/list resources/list)))
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
