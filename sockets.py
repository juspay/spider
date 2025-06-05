import os
import concurrent.futures
import json
import datetime
import socket
import threading
import signal
import sys

# Path for the Unix Domain Socket
socket_path = '/tmp/fdep.sock'

# Ensure socket file doesn't exist
if os.path.exists(socket_path):
    os.unlink(socket_path)

# Global data storage
data = dict()

def handle_client(conn, client_addr):
    """Handle a client connection"""
    buffer = b''
    path = None
    
    # Receive data until we get a complete message
    while True:
        try:
            chunk = conn.recv(4096)
            if not chunk:
                break
            
            buffer += chunk
            
            # Process complete messages
            if b'\n' in buffer and path is None:
                # First newline separates path from data
                header, buffer = buffer.split(b'\n', 1)
                path = header.decode('utf-8')
                
                # Continue receiving if we don't have complete data yet
                if not buffer:
                    continue
            
            # If we have a path and data, process it
            if path is not None:
                try:
                    # Check if this is a file dump
                    file_dump = any(
                        i in path
                        for i in [
                            "module_imports",
                            "function_code",
                            "types_code",
                            "class_code",
                            "instance_code",
                            "fieldUsage",
                            "typeUpdates",
                            ".types.parser.",
                            ".function_instance_mapping",
                            "module_apis",
                            ".type.typechecker.json"
                        ]
                    )
                    
                    if file_dump:
                        obj = json.loads(buffer)
                        os.makedirs(path[1:].rsplit("/", 1)[0], exist_ok=True)
                        with open(path[1:], "w") as f:
                            f.write(json.dumps(obj, indent=4))
                        # Reset for next message
                        buffer = b''
                        path = None
                    else:
                        try:
                            obj = json.loads(buffer)
                            if isinstance(obj, list):
                                # Handle batch data
                                for item in obj:
                                    if data.get(path) is None:
                                        data[path] = dict()
                                    if data[path].get(item.get("key")) is None:
                                        data[path][item.get("key")] = []
                                    data[path][item.get("key")].append(item)
                            else:
                                # Handle single item
                                if data.get(path) is None:
                                    data[path] = dict()
                                if data[path].get(obj.get("key")) is None:
                                    data[path][obj.get("key")] = []
                                data[path][obj.get("key")].append(obj)
                            # Reset for next message
                            buffer = b''
                            path = None
                        except json.JSONDecodeError:
                            # Incomplete JSON, continue receiving
                            continue
                
                except Exception as e:
                    print(f"Error processing message: {e}")
                    # Reset for next message
                    buffer = b''
                    path = None
        
        except Exception as e:
            print(f"Error receiving data: {e}")
            break
    
    # Connection closed, drain data for this module if we have a path
    if path is not None:
        try:
            a = datetime.datetime.now()
            drain_for_module(path)
            b = datetime.datetime.now()
            delta = b - a
            print("time taken to dump: ", path[1:], delta)
        except Exception as e:
            print(f"Error draining data: {e}")
    
    conn.close()

def drain_for_module(path):
    """Drain data for a specific module"""
    global data
    if data.get(path) is not None:
        v = data.get(path)
        try:
            process_fdep_output(path, v)
            del data[path]
        except Exception as e:
            print("draining", e)

def process_fdep_output(k, v):
    """Process and save output data"""
    if not os.path.isfile(k[1:]):
        os.makedirs(k[1:].rsplit("/", 1)[0], exist_ok=True)
        with open(k[1:], "w") as f:
            json.dump(v, f, indent=4)
    else:
        try:
            with open(k[1:], "r") as f:
                alreadyPresentdict = json.load(f)
                newDict = dict(v, **alreadyPresentdict)
                print(v.keys(), alreadyPresentdict.keys())
                with open(k[1:], "w") as ff:
                    json.dump(newDict, ff, indent=4)
        except Exception as e:
            print(e)
            with open(k[1:], "w") as f:
                json.dump(v, f, indent=4)

def drain_all_data():
    """Drain all data before exiting"""
    global data
    with concurrent.futures.ThreadPoolExecutor(max_workers=20) as executor:
        future_to_file = {
            executor.submit(process_fdep_output, k, v): (k, v)
            for (k, v) in data.items()
        }
        for future in concurrent.futures.as_completed(future_to_file):
            pass
    print(json.dumps(list(data.keys())))

def signal_handler(sig, frame):
    """Handle signals to gracefully shut down"""
    print(f"Received signal {sig}, shutting down...")
    drain_all_data()
    if os.path.exists(socket_path):
        os.unlink(socket_path)
    sys.exit(0)

def start_server():
    """Start the Unix Domain Socket server"""
    # Register signal handlers
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    # Create socket
    server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    server.bind(socket_path)
    server.listen(100)  # Allow up to 100 connections
    
    # Set socket permissions to allow all users to connect
    os.chmod(socket_path, 0o777)
    
    print(f"Unix Domain Socket server started on {socket_path}")
    
    try:
        while True:
            # Accept connections
            conn, addr = server.accept()
            print(f"New connection")
            
            # Handle client in a new thread
            client_thread = threading.Thread(target=handle_client, args=(conn, addr))
            client_thread.daemon = True
            client_thread.start()
    
    except KeyboardInterrupt:
        print("Server shutting down...")
    
    finally:
        drain_all_data()
        server.close()
        if os.path.exists(socket_path):
            os.unlink(socket_path)

def find_free_socket_path():
    """Find a free socket path that doesn't exist yet"""
    base_path = '/tmp/fdep'
    os.makedirs(base_path, exist_ok=True)
    
    # Try to find a unique socket path
    for i in range(1000):
        path = f"{base_path}/fdep_{i}.sock"
        if not os.path.exists(path):
            return path
    
    # Fallback to a timestamp-based path if all else fails
    import time
    return f"{base_path}/fdep_{int(time.time())}.sock"

if __name__ == "__main__":
    # Find a free socket path
    socket_path = find_free_socket_path()
    
    # Create a file with the socket path for clients to discover
    with open("fdep_socket", "w") as f:
        f.write(socket_path)
    
    # Print the export command for the user to run
    print(f"Run the following command in your shell to set the socket path:")
    print(f"export FDEP_SOCKET_PATH={socket_path}")
    
    # Start the server
    start_server()
