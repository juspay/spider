import asyncio
import os
import json
import socket
import datetime
import gc
import signal
import sys
import threading
from pathlib import Path

gc.disable()


def find_free_socket_path():
    """Find a free socket path that doesn't exist yet"""
    base_path = "/tmp/fdep"
    os.makedirs(base_path, exist_ok=True)

    # Try to find a unique socket path
    for i in range(1000):
        path = f"{base_path}/fdep_{i}.sock"
        if not os.path.exists(path):
            return path

    # Fallback to a timestamp-based path if all else fails
    import time

    return f"{base_path}/fdep_{int(time.time())}.sock"


async def handle_client(reader, writer):
    """Handle a client connection asynchronously"""
    a = datetime.datetime.now()
    buffer = list()
    path = None

    try:
        # Read all data first
        data = await reader.read()
        if not data:
            return

        message = data.decode("utf-8")

        # Split on the first occurrence of "****"
        if "****" not in message:
            print(f"Invalid message format - no '****' separator found")
            return

        path, data_content = message.split("****", 1)
        path = path.strip()

        # Check if this is a file dump
        file_dump = False
        for i in [
            "module_imports",
            "function_code",
            "types_code",
            "class_code",
            "instance_code",
            "fieldUsage",
            "typeUpdates",
            "types.parser.json",
            "module_apis.json",
            "function_instance_mapping.json",
            "type.typechecker.json",
        ]:
            if i in path:
                file_dump = True
                break

        if file_dump:
            # For file dumps, write directly to file
            os.makedirs(path[1:].rsplit("/", 1)[0], exist_ok=True)

            with open(path[1:], "w") as f:
                f.write(data_content)
        else:
            # For streaming data, add to buffer
            data_content_list = message.strip().split("\n")
            for i in data_content_list:
                try:
                    buffer.append(i.split("****")[1])
                except Exception as e:
                    print(e)

            # Process buffer when it gets large enough
            if len(buffer) > 400:
                await asyncio.get_event_loop().run_in_executor(
                    None, process_fdep_output, path, buffer
                )
                buffer = []
                gc.collect()

    except Exception as e:
        print(f"Error handling client: {e}")

    finally:
        # Process any remaining buffer
        if buffer and path:
            await asyncio.get_event_loop().run_in_executor(
                None, process_fdep_output, path, buffer
            )

        writer.close()
        await writer.wait_closed()

        if path:
            b = datetime.datetime.now()
            delta = b - a
            print("time taken to dump:", path[1:], delta)


def process_fdep_output(k, v):
    """Process and save output data"""
    if not v:  # Skip if buffer is empty
        return
    output_path = k[1:]

    if not os.path.isfile(output_path):
        os.makedirs(output_path.rsplit("/", 1)[0], exist_ok=True)
        with open(output_path, "w") as f:
            f.write("\n".join(v) + "\n")
    else:
        try:
            with open(output_path, "a") as f:
                f.write("\n".join(v) + "\n")
        except Exception as e:
            print(f"Error writing to file {output_path}: {e}")


async def start_socket_server():
    """Start the Unix Domain Socket server"""
    socket_path = find_free_socket_path()

    # Ensure socket file doesn't exist
    if os.path.exists(socket_path):
        os.unlink(socket_path)

    # Write socket path to a file that can be read by clients
    with open("server.pid", "w") as f:
        f.write(socket_path)

    # Print the export command for the user to run
    print(f"Run the following command in your shell to set the socket path:")
    print(f"export FDEP_SOCKET_PATH={socket_path}")

    # Start the server
    server = await asyncio.start_unix_server(handle_client, path=socket_path)

    # Set socket permissions to allow all users to connect
    os.chmod(socket_path, 0o777)

    print(f"Unix Domain Socket server started on {socket_path}")

    # Setup signal handlers for graceful shutdown
    def signal_handler(sig, frame):
        print(f"Received signal {sig}, shutting down...")
        server.close()
        if os.path.exists(socket_path):
            os.unlink(socket_path)
        sys.exit(0)

    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    try:
        async with server:
            await server.serve_forever()
    except KeyboardInterrupt:
        print("Server shutting down...")
    finally:
        server.close()
        await server.wait_closed()
        if os.path.exists(socket_path):
            os.unlink(socket_path)


if __name__ == "__main__":
    try:
        asyncio.run(start_socket_server())
    except KeyboardInterrupt:
        print("Server stopped")
