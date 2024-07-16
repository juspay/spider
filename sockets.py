import asyncio
import os
import concurrent.futures
import json
import asyncio
import json
import websockets
from aiohttp import web
# nix-shell -p python311Packages.websockets
data = dict()

async def handler(websocket, path):
    try:
        async for message in websocket:
            try:
                obj = json.loads(message)
                if data.get(path) == None:
                    data[path] = dict()
                if data[path].get(obj.get("key")) == None:
                    data[path][obj.get("key")] = []
                data[path][obj.get("key")].append(obj)
            except Exception as e:
                print(e)
    except websockets.exceptions.ConnectionClosed as e:
        print(e,path)
    except Exception as e:
        print(e)

def process_fdep_output(k,v):
    os.makedirs(k[1:].replace(".json",""), exist_ok=True)
    with open(k[1:],'w') as f:
        json.dump(v,f,indent=4)

async def drain_data(request):
    global data
    with concurrent.futures.ThreadPoolExecutor(max_workers=20) as executor:
        future_to_file = {executor.submit(process_fdep_output, k,v): (k,v) for (k,v) in data.items()}
        for future in concurrent.futures.as_completed(future_to_file):
            pass
    print(json.dumps(list(data.keys())))
    exit()

async def start_websocket_server():
    async with websockets.serve(handler, "localhost", 8000,ping_interval=None,ping_timeout=None,close_timeout=None,max_queue=1000):
        print("WebSocket server started on ws://localhost:8000")
        await asyncio.Future()

async def start_http_server():
    app = web.Application()
    app.router.add_get('/drain', drain_data)
    runner = web.AppRunner(app)
    await runner.setup()
    site = web.TCPSite(runner, 'localhost', 8080)
    await site.start()
    print("HTTP server started on http://localhost:8080")

async def main():
    await asyncio.gather(
        start_websocket_server(),
        start_http_server()
    )

if __name__ == "__main__":
    asyncio.run(main())