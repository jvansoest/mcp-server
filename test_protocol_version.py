#!/usr/bin/env python3
import json
import requests

def test_protocol_version_validation():
    """Test protocol version validation in initialize request."""
    
    url = "http://localhost:8080"
    headers = {"Content-Type": "application/json"}
    
    # Test 1: Valid protocol version
    print("Testing valid protocol version...")
    
    init_request_valid = {
        "jsonrpc": "2.0",
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test-client", "version": "1.0.0"}
        },
        "id": 1
    }
    
    try:
        response = requests.post(url, json=init_request_valid, headers=headers, timeout=5)
        print(f"✓ Valid version response: {response.status_code}")
        if response.status_code == 200:
            result = response.json()
            if "result" in result:
                print("✓ Valid protocol version accepted")
            else:
                print(f"✗ Error: {result.get('error', {}).get('message', '')}")
        
    except requests.exceptions.RequestException as e:
        print(f"✗ Valid version request failed: {e}")
    
    # Test 2: Invalid protocol version
    print("\nTesting invalid protocol version...")
    
    init_request_invalid = {
        "jsonrpc": "2.0",
        "method": "initialize",
        "params": {
            "protocolVersion": "2023-01-01",  # Invalid version
            "capabilities": {},
            "clientInfo": {"name": "test-client", "version": "1.0.0"}
        },
        "id": 2
    }
    
    try:
        response = requests.post(url, json=init_request_invalid, headers=headers, timeout=5)
        print(f"✓ Invalid version response: {response.status_code}")
        if response.status_code == 200:
            result = response.json()
            if "error" in result:
                print(f"✓ Invalid protocol version rejected: {result['error']['message']}")
            else:
                print("✗ Invalid protocol version should have been rejected")
        
    except requests.exceptions.RequestException as e:
        print(f"✗ Invalid version request failed: {e}")
    
    # Test 3: Missing protocol version
    print("\nTesting missing protocol version...")
    
    init_request_missing = {
        "jsonrpc": "2.0",
        "method": "initialize",
        "params": {
            "capabilities": {},
            "clientInfo": {"name": "test-client", "version": "1.0.0"}
        },
        "id": 3
    }
    
    try:
        response = requests.post(url, json=init_request_missing, headers=headers, timeout=5)
        print(f"✓ Missing version response: {response.status_code}")
        if response.status_code == 200:
            result = response.json()
            if "error" in result:
                print(f"✓ Missing protocol version rejected: {result['error']['message']}")
            else:
                print("✗ Missing protocol version should have been rejected")
        
    except requests.exceptions.RequestException as e:
        print(f"✗ Missing version request failed: {e}")

if __name__ == "__main__":
    test_protocol_version_validation()