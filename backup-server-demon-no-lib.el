(require 'json)

(defvar jsonrpc-server-process nil)

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
                 ""))
         (json-request (if (> (length body) 0)
                          (json-parse-string body :object-type 'alist)
                        nil)))
    
    (when json-request
      (let* ((method (alist-get 'method json-request))
             (params (alist-get 'params json-request))
             (id (alist-get 'id json-request))
             (response (cond
                       ((string= method "hello")
                        `((jsonrpc . "2.0")
                          (result . "hello world")
                          (id . ,id)))
                       (t
                        `((jsonrpc . "2.0")
                          (error . ((code . -32601)
                                   (message . "Method not found")))
                          (id . ,id))))))
        
        (let ((response-json (json-encode response)))
          (process-send-string proc
                              (format "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s"
                                     (length response-json)
                                     response-json))))))
  
  (process-send-eof proc))

(defun jsonrpc-start-server-daemon (port)
  "Start JSON-RPC server daemon on PORT."
  (setq jsonrpc-server-process
        (make-network-process
         :name "jsonrpc-server"
         :service port
         :server t
         :host 'local
         :family 'ipv4
         :filter 'jsonrpc-server-handler))
  
  (message "JSON-RPC server started on port %d" port)
  
  ;; Keep server running
  (while t
    (accept-process-output nil 1)))

;; Start server
(jsonrpc-start-server-daemon 8080)