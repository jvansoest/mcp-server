(require 'json)

(defun jsonrpc-http-call (host port method params &optional id)
  "Make a JSON-RPC HTTP call to HOST:PORT."
  (let* ((request-id (or id (random 1000)))
         (request-data `((jsonrpc . "2.0")
                        (method . ,method)
                        (params . ,params)
                        (id . ,request-id)))
         (json-request (json-encode request-data))
         (proc (make-network-process
                :name "jsonrpc-client"
                :host host
                :service port
                :family 'ipv4))
         (http-request (format "POST / HTTP/1.1\r\nHost: %s:%d\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s"
                              host port (length json-request) json-request))
         (response ""))
    
    (set-process-filter proc
                       (lambda (process output)
                         (setq response (concat response output))))
    
    (process-send-string proc http-request)
    (process-send-eof proc)
    
    ;; Wait for response
    (while (process-live-p proc)
      (accept-process-output proc 0.1))
    
    ;; Parse response
    (when (string-match "\r\n\r\n\\(.*\\)" response)
      (let ((body (match-string 1 response)))
        (when (> (length body) 0)
          (json-parse-string body :object-type 'alist))))))

(defun jsonrpc-http-hello ()
  "Call hello method via HTTP."
  (interactive)
  (let ((response (jsonrpc-http-call "localhost" 8080 "hello" [])))
    (if response
        (if (alist-get 'result response)
            (message "Server response: %s" (alist-get 'result response))
          (message "Error: %s" (alist-get 'error response)))
      (message "No response received"))))

;; Test function
(defun test-http-client ()
  "Test the HTTP JSON-RPC client."
  (interactive)
  (jsonrpc-http-hello))