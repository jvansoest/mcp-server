(require 'json-rpc-server)

(defvar jsonrpc-server-process nil)

(defun hello ()
  "Handle hello method calls."
  "hello world")

(defun jsonrpc-server-handler (proc string)
  "Handle HTTP requests for JSON-RPC."
  (let* ((lines (split-string string "\r\n"))
         (request-line (car lines))
         (headers (cdr lines))
         (body-start (cl-position "" headers :test 'string=))
         (body (if body-start
                   (mapconcat 'identity 
                             (nthcdr (1+ body-start) headers) 
                             "\r\n")
                 "")))
    
    (when (> (length body) 0)
      (let* ((response-json (json-rpc-server-handle body '(hello)))
             (response-length (length response-json)))
        (process-send-string proc
                            (format "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s"
                                   response-length
                                   response-json))))
  
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
         :service 8080
         :server t
         :host 'local
         :family 'ipv4
         :filter 'jsonrpc-server-handler))
  
  (message "JSON-RPC server started on port %d" 8080))

(defun jsonrpc-stop-server-daemon ()
  "Stop the JSON-RPC server daemon."
  (interactive)
  (if jsonrpc-server-process
      (progn
        (delete-process jsonrpc-server-process)
        (setq jsonrpc-server-process nil)
        (message "JSON-RPC server stopped"))
    (message "No JSON-RPC server is running")))

;; Start server when loaded interactively
;; (when (not noninteractive)
  ;; (jsonrpc-start-server-daemon 8080))
