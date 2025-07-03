(require 'json)

(defun test-mcp-initialization ()
  "Test MCP initialization sequence."
  (let* ((init-request `((jsonrpc . "2.0")
                        (method . "initialize")
                        (params . ((protocolVersion . "2024-11-05")
                                  (capabilities . ((roots . ())))
                                  (clientInfo . ((name . "test-client")
                                               (version . "1.0.0")))))
                        (id . 1)))
         (hello-request `((jsonrpc . "2.0")
                         (method . "hello")
                         (params . [])
                         (id . 2))))
    
    (message "Testing MCP initialization...")
    
    ;; Test initialization
    (let ((response (jsonrpc-test-server-process (json-encode init-request))))
      (message "Initialize response: %s" response))
    
    ;; Test hello after initialization
    (let ((response (jsonrpc-test-server-process (json-encode hello-request))))
      (message "Hello response: %s" response))))

(defun jsonrpc-test-server-process (request)
  "Process JSON-RPC request using the test server."
  (json-rpc-server-handle request '(initialize hello)))

;; Run the test
(test-mcp-initialization)