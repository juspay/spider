import asyncio
import os
import concurrent.futures
import json
import asyncio
import json
import websockets
from aiohttp import web
import datetime

# nix-shell -p python311Packages.websockets
data = dict()


async def handler(websocket, path):
    try:
        file_dump = False
        for i in [
            "module_imports",
            "function_code",
            "types_code",
            "class_code",
            "instance_code",
            "fieldUsage",
            "typeUpdates",
            ".types.parser.",
        ]:
            if i in path:
                file_dump = True

        async for message in websocket:
            try:
                if file_dump:
                    obj = json.loads(message)
                    os.makedirs(path[1:].rsplit("/", 1)[0], exist_ok=True)
                    with open(path[1:], "w") as f:
                        f.write(json.dumps(obj, indent=4))
                        f.close()
                else:
                    obj = json.loads(message)
                    if data.get(path) == None:
                        data[path] = dict()
                    if data[path].get(obj.get("key")) == None:
                        data[path][obj.get("key")] = []
                    data[path][obj.get("key")].append(obj)
            except Exception as e:
                print(e)
    except websockets.exceptions.ConnectionClosed as e:
        a = datetime.datetime.now()
        drain_for_module(path)
        b = datetime.datetime.now()
        delta = b - a
        print("time taken to dump: ", path[1:], delta)
    except Exception as e:
        print(e)


def drain_for_module(path):
    global data
    if data.get(path) != None:
        v = data.get(path)
        try:
            process_fdep_output(path, v)
            del data[path]
        except Exception as e:
            print("draining", e)


def process_fdep_output(k, v):
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


async def drain_data(request):
    global data
    with concurrent.futures.ThreadPoolExecutor(max_workers=20) as executor:
        future_to_file = {
            executor.submit(process_fdep_output, k, v): (k, v)
            for (k, v) in data.items()
        }
        for future in concurrent.futures.as_completed(future_to_file):
            pass
    print(json.dumps(list(data.keys())))
    exit(0)


async def start_websocket_server():
    async with websockets.serve(
        handler,
        "::1",
        4444,
        ping_interval=None,
        ping_timeout=None,
        close_timeout=None,
        max_queue=1000,
    ):
        print("WebSocket server started on ws://::1:4444")
        await asyncio.Future()


async def start_http_server():
    app = web.Application()
    app.router.add_get("/drain", drain_data)
    runner = web.AppRunner(app)
    await runner.setup()
    site = web.TCPSite(runner, "localhost", 8080)
    await site.start()
    print("HTTP server started on http://localhost:8080")


async def main():
    await asyncio.gather(start_websocket_server(), start_http_server())


if __name__ == "__main__":
    asyncio.run(main())
