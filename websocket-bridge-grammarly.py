import asyncio
import playwright.async_api
import threading
import socket
import websocket_bridge_python
import json
import re
import sys

from http.server import BaseHTTPRequestHandler, HTTPServer
from playwright.async_api import async_playwright, TimeoutError as PlaywrightTimeoutError


# get system free port
def get_free_port():
    import socket

    sock = socket.socket()
    sock.bind(("", 0))
    port = sock.getsockname()[1]
    sock.close()
    return port

# define a global free port for Grammarly Demo.
port = get_free_port()


# define a global object to save analyse result.
grammarly_response = {
    "raw": "",
    "infos": [],
}

# eval in emacs and log the command.
async def run_and_log(cmd):
    print(cmd, flush=True)
    await bridge.eval_in_emacs(cmd)

# define a BaseHTTPRequestHandler to show Grammarly Demo page.
class GrammarlyDemo(BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()
        self.wfile.write(
            bytes(
                '<grammarly-editor-plugin> <textarea rows="10"> </textarea> </grammarly-editor-plugin> <script src="https://unpkg.com/@grammarly/editor-sdk@2.0?clientId=client_K8kCkKgfciCQs8uioHa1wu"></script>',
                "utf-8",
            )
        )

# run Grammarly web demo.
def run_grammarly_demo():
    hostName = "127.0.0.1"
    webServer = HTTPServer((hostName, port), GrammarlyDemo)
    print("Server started http://%s:%s" % (hostName, port), flush=True)
    try:
        webServer.serve_forever()
    except KeyboardInterrupt:
        pass

# define a handler for chrome received websockets.
async def handle_websocket_frame_received(params):
    payload = params["response"]["payloadData"]
    await handle_ws_msg(payload)

# check a string if a json.
def is_json(msg):
    try:
        json.loads(msg)
    except ValueError as e:
        return False
    return True


# handle websocket mesage, send a request for add overlays on error position.
async def handle_ws_msg(msg):
    if not is_json(msg):
        return
    obj = json.loads(msg)
    if "point" not in obj:
        return
    category = obj["category"]
    begin = obj["begin"] + 1
    end = obj["end"] + 1
    print(obj)
    grammarly_response["infos"].append(obj)
    cmd = f'(websocket-bridge-grammarly-overlay-from "{category}" {begin} {end})'
    await run_and_log(cmd)
            
async def login_grammarly():
    await page.type("textarea", " ")
    element = await page.wait_for_selector("button")
    await element.click()
    buttons = await page.wait_for_selector("button")
    connect_button = await page.wait_for_selector('text="Connect your Grammarly account"')
    await connect_button.click()
    while len(context.pages) != 2:
        await page.wait_for_timeout(500)
    connect_page = context.pages[1]
    connect_button = await connect_page.wait_for_selector('text="Connect"')
    await connect_button.click()
    f12 = await page.context.new_cdp_session(page)
    await f12.send("Network.enable")
    await f12.send("Page.enable")
    f12.on("Network.webSocketFrameReceived", handle_websocket_frame_received)

# using playwright to connect Grammarly demo and login with account.
async def connect_grammarly(playwright):
    global page, context
    browser_type = playwright.chromium
    browser = await browser_type.launch( headless=False)
    context = await browser.new_context()
    need_login = await bridge.get_emacs_var("websocket-bridge-grammarly-need-login")
    cookie_array=[]
    if need_login:
        from pycookiecheat import chrome_cookies
        cookies = chrome_cookies("https://app.grammarly.com/")
        cookie_array = [
            {"name": x, "value": y, "path": "/", "domain": ".grammarly.com"}
            for x, y in cookies.items()
        ]
    await context.add_cookies(cookie_array)
    page = await context.new_page()
    await page.goto("http://127.0.0.1:" + str(port))
    if need_login:
        await login_grammarly()
     
    

# dispatch message recived from Emacs.
async def on_message(message):
    info = json.loads(message)
    cmd = info[1][0].strip()
    sentence_str = info[1][1]
    point = info[1][2]
    if cmd == "analyze":
        await remove_overlays()
        await analyze_sentence(sentence_str)
    elif cmd == "getInfo":
        await get_analyze_info(point)
    elif cmd == "listInfos":
        await list_analyze_infos()
    else:
        print("not fount dispatcher", flush=True)
        
# list all analyze informations.
async def list_analyze_infos():
    html = "".join([parse_simple_html(info) for info in grammarly_response["infos"]])
    await run_and_log(f'(websocket-bridge-grammarly-render "{html}")')
    
 # extract simple html from websocket json.
def parse_simple_html(json):
    title = get_value_by_key(json, "point")
    transforms = json["transforms"] if "transforms" in json else []
    transform_html = ""
    for transform in transforms:
        transform = re.sub(
            r"<span class='gr_grammar_del'>(.*?)<\/span>",
            r"<del>\1</del> -> ",
            transform.replace('"', "'")
        )
        transform_html += f"<p>{transform}</p>"
    html = f"""<h1>{title}</h1>
<h2>transforms</h2>
{transform_html} 
"""
    return html
        
        
# send request to Emacs and remove overlays.
async def remove_overlays():
    cmd = '(remove-overlays)'
    await run_and_log(cmd)
    

# get current piont the analyze info.
async def get_analyze_info(point):
    for info in grammarly_response["infos"]:
        if point > info["begin"] and point < info["end"]:
            html = extractHtml(info)
            await run_and_log(f'(websocket-bridge-grammarly-render "{html}")')
            print(4)
            break
        

def get_value_by_key(json, key):
    return json[key].replace('"', "'") if key in json else ""

# extract html from websocket json.
def extractHtml(json):
    title = get_value_by_key(json, "point")
    transforms = json["transforms"] if "transforms" in json else []
    details = get_value_by_key(json, "details")
    explanation = get_value_by_key(json, "explanation")
    examples = get_value_by_key(json, "examples")
    transform_html = ""
    for transform in transforms:
        transform = re.sub(
            r"<span class='gr_grammar_del'>(.*?)<\/span>",
            r"<del>\1</del> -> ",
            transform.replace('"', "'")
        )
        transform_html += f"<p>{transform}</p>"
    html = f"""<h1>{title}</h1>
<h2>transforms</h2>
{transform_html} 
<h2>details</h2>
{details} 
<h2>explanation</h2>
{explanation}
<h2>examples</h2>
{examples}"""
    return html

# type message in Grammarly Demo to analyze it.
async def analyze_sentence(sentence):
    if grammarly_response["raw"] != sentence:
        await clear(page, "textarea")
        await page.type("textarea", sentence)

# clear page's child dom by selector.
async def clear(page, selector):
    await page.evaluate(
        'selector => document.querySelector(selector).value = ""', selector
    )

# main
async def main():
    global bridge
    t = threading.Thread(target=run_grammarly_demo)
    t.start()
    async with async_playwright() as playwright:
        try:
            bridge = websocket_bridge_python.bridge_app_regist(on_message)
            await asyncio.gather(connect_grammarly(playwright), bridge.start())
        except TimeoutError:
            print("Timeout!")


asyncio.run(main())
