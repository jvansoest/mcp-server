#!/usr/bin/env python3
import json
import requests
import time

def test_mcp_initialization():
    """Test MCP initialization sequence via HTTP."""
    
    url = "http://localhost:3000/mcp"
    headers = {"Content-Type": "application/json"}
    
    # Test 1: Initialize the server
    print("Testing MCP initialization...")

    init_request = {
        "jsonrpc": "2.0",
        "method": "initialize",
        "params": {
            "protocolVersion": "2025-06-18",
            "capabilities": {
                "roots": {}
            },
            "clientInfo": {
                "name": "test-client",
                "version": "1.0.0"
            }
        },
        "id": 0,
    }
    
    try:
        response = requests.post(url, json=init_request, headers=headers, timeout=5)
        print(f"✓ Initialize response: {response.status_code}")

        if response.status_code == 200:
            result = response.json()
            print(f"✓ Server capabilities: {result}")

    except requests.exceptions.RequestException as e:
        print(f"✗ Initialize request failed: {e}")
        return False
    
    # # Test 2: Send initialized notification
    # print("\nSending initialized notification...")

    # initialized_notification = {
    #     "jsonrpc": "2.0",
    #     "method": "notifications/initialized"
    # }

    # try:
    #     response = requests.post(url, json=initialized_notification, headers=headers, timeout=5)
    #     print(f"✓ Initialized notification: {response.status_code}")
    #     print(f"✓ Response body: {response.text}")

    # except requests.exceptions.RequestException as e:
    #     print(f"✗ Initialized notification failed: {e}")
    #     return False

    # # Test 2b: Try hello before initialized (should fail)
    # print("\nTesting hello method before initialized...")

    # hello_request_early = {
    #     "jsonrpc": "2.0",
    #     "method": "hello",
    #     "params": [],
    #     "id": 2
    # }

    # try:
    #     response = requests.post(url, json=hello_request_early, headers=headers, timeout=5)
    #     print(f"✓ Hello early response: {response.status_code}")
    #     print(f"✓ Response body: {response.text}")

    # except requests.exceptions.RequestException as e:
    #     print(f"✗ Hello early request failed: {e}")
    #     return False
    
    # # Test 3: Call hello method
    # print("\nTesting hello method...")

    # hello_request = {
    #     "jsonrpc": "2.0",
    #     "method": "hello",
    #     "params": [],
    #     "id": 2
    # }

    # try:
    #     response = requests.post(url, json=hello_request, headers=headers, timeout=5)
    #     print(f"✓ Hello response: {response.status_code}")
    #     print(f"✓ Response body: {response.text}")

    #     if response.status_code == 200:
    #         result = response.json()
    #         print(f"✓ Hello result: {result.get('result', '')}")

    # except requests.exceptions.RequestException as e:
    #     print(f"✗ Hello request failed: {e}")
    #     return False

    return True

if __name__ == "__main__":
    test_mcp_initialization()
