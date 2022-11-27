"""
websocket-bridge python extension for grammarly.
"""
import asyncio
import json
import re
import socket
import threading
from http.server import BaseHTTPRequestHandler, HTTPServer

import websocket_bridge_python
from playwright.async_api import async_playwright

def get_free_port()->int:
    """get system free por"""
    sock = socket.socket()
    sock.bind(("", 0))
    port = sock.getsockname()[1]
    sock.close()
    return port

# define a global free port for Grammarly Demo.
grammarly_demo_port = get_free_port()

# define a global object to save analyse result.
grammarly_response = {
    "raw": "",
    "infos": [],
}

async def run_and_log(cmd:str):
    """eval in emacs and log the command."""
    print(cmd, flush=True)
    await BRIDGE.eval_in_emacs(cmd)

class GrammarlyDemo(BaseHTTPRequestHandler):
    """define a BaseHTTPRequestHandler to show Grammarly Demo page."""
    def do_GET(self):
        """return grammarly demo html."""
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()
        self.wfile.write(bytes('<grammarly-editor-plugin><textarea rows="10"></textarea></grammarly-editor-plugin><script src="https://unpkg.com/@grammarly/editor-sdk@2.0?clientId=client_K8kCkKgfciCQs8uioHa1wu"></script>', "utf-8"))

def run_grammarly_demo():
    """run Grammarly web demo."""
    host_name = "127.0.0.1"
    web_server = HTTPServer((host_name, grammarly_demo_port), GrammarlyDemo)
    print(f"Server started http://{host_name}:{grammarly_demo_port}", flush=True)
    try:
        web_server.serve_forever()
    except KeyboardInterrupt:
        pass

async def handle_websocket_frame_received(params:dict):
    """define a handler for chrome received websockets."""
    payload = params["response"]["payloadData"]
    await handle_ws_msg(payload)

def is_json(msg:str)->bool:
    """check a string if a json."""
    try:
        json.loads(msg)
    except ValueError:
        return False
    return True

async def handle_ws_msg(msg:str):
    """handle websocket mesage, send a request for add overlays on error position."""
    if not is_json(msg):
        return
    obj = json.loads(msg)
    if "point" not in obj:
        return
    else:
        print(obj)
    category = obj["category"]
    begin = obj["begin"] + 1
    end = obj["end"] + 1
    grammarly_response["infos"].append(obj)
    cmd = f'(websocket-bridge-grammarly-overlay-from "{category}" {begin} {end})'
    await run_and_log(cmd)

async def login_grammarly():
    """Login grammarly with cookie"""
    await PAGE.type("textarea", " ")
    element = await PAGE.wait_for_selector("button")
    await element.click()
    connect_button = await PAGE.wait_for_selector(
        'text="Connect your Grammarly account"'
    )
    await connect_button.click()
    while len(CONTEXT.pages) != 2:
        await PAGE.wait_for_timeout(500)
    connect_page = CONTEXT.pages[1]
    connect_button = await connect_page.wait_for_selector('text="Connect"')
    await connect_button.click()
    f12 = await PAGE.context.new_cdp_session(PAGE)
    await f12.send("Network.enable")
    await f12.send("Page.enable")
    f12.on("Network.webSocketFrameReceived", handle_websocket_frame_received)

async def connect_grammarly(playwright):
    """using playwright to connect Grammarly demo and login with account."""
    global PAGE, CONTEXT
    browser_type = playwright.chromium
    browser = await browser_type.launch(headless=False)
    CONTEXT = await browser.new_context()
    need_login = await BRIDGE.get_emacs_var("websocket-bridge-grammarly-need-login")
    cookie_array = []
    if need_login:
        from pycookiecheat import chrome_cookies

        cookies = chrome_cookies("https://app.grammarly.com/")
        cookie_array = [
            {"name": x, "value": y, "path": "/", "domain": ".grammarly.com"}
            for x, y in cookies.items()
        ]
    await CONTEXT.add_cookies(cookie_array)
    PAGE = await CONTEXT.new_page()
    await PAGE.goto("http://127.0.0.1:" + str(grammarly_demo_port))
    if need_login:
        await login_grammarly()

async def on_message(message):
    """dispatch message recived from Emacs."""
    info = json.loads(message)
    cmd = info[1][0].strip()
    sentence_str = info[1][1]
    point = info[1][2]
    if cmd == "analyze":
        await remove_overlays()
        await analyze_sentence(sentence_str)
    elif cmd == "get_details":
        await get_details(point)
    elif cmd == "list_all":
        await list_all()
    else:
        print("not fount dispatcher", flush=True)

async def list_all():
    """list all analyze informations."""
    html = "".join([parse_simple_html(info) for info in grammarly_response["infos"]])
    await run_and_log(f'(websocket-bridge-grammarly-render "{html}")')

def parse_simple_html(ws_json:dict)->str:
    """extract simple html from websocket json."""
    title = get_value_by_key(ws_json, "point")
    transforms = ws_json["transforms"] if "transforms" in ws_json else []
    transform_html = ""
    for transform in transforms:
        transform = re.sub(
            r"<span class='gr_grammar_del'>(.*?)<\/span>",
            r"<del>\1</del> -> ",
            transform.replace('"', "'"))
        transform_html += f"<p>{transform}</p>"
    html = f"""<h1>{title}</h1>
    <h2>transforms</h2>
    {transform_html}
    """
    return html

async def remove_overlays():
    """send request to Emacs and remove overlays."""
    cmd = "(remove-overlays)"
    await run_and_log(cmd)

async def get_details(point):
    """get current piont the analyze info."""
    for info in grammarly_response["infos"]:
        if info["end"] > point > info["begin"]:
            html = extract_html(info)
            await run_and_log(f'(websocket-bridge-grammarly-render "{html}")')
            print(4)
            break

def get_value_by_key(json_obj, key):
    "Get Json value by key"
    return json_obj[key].replace('"', "'") if key in json_obj else ""

def extract_html(json_obj):
    """extract html from websocket json."""
    title = get_value_by_key(json_obj, "point")
    transforms = json_obj["transforms"] if "transforms" in json_obj else []
    details = get_value_by_key(json_obj, "details")
    explanation = get_value_by_key(json_obj, "explanation")
    examples = get_value_by_key(json_obj, "examples")
    transform_html = ""
    for transform in transforms:
        transform = re.sub(
            r"<span class='gr_grammar_del'>(.*?)<\/span>",
            r"<del>\1</del> -> ",
            transform.replace('"', "'"),
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

async def analyze_sentence(sentence):
    """type message in Grammarly Demo to analyze it."""
    if grammarly_response["raw"] != sentence:
        await clear(PAGE, "textarea")
        await PAGE.type("textarea", sentence)

async def clear(page, selector):
    """clear page's child dom by selector."""
    await page.evaluate(
        'selector => document.querySelector(selector).value = ""', selector
    )

async def main():
    """main"""
    global BRIDGE
    threading.Thread(target=run_grammarly_demo).start()
    async with async_playwright() as playwright:
        try:
            BRIDGE = websocket_bridge_python.bridge_app_regist(on_message)
            await asyncio.gather(connect_grammarly(playwright), BRIDGE.start())
        except TimeoutError:
            print("Timeout!")

asyncio.run(main())
