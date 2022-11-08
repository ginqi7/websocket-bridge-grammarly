import asyncio
from playwright.async_api import async_playwright
from pycookiecheat import chrome_cookies
import threading
import socket
import websocket_bridge
import json
import re
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer

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
analyseInfo = {
    "raw": "",
    "infos": [],
}

# eval in emacs and log the command.
async def runAndLog(cmd):
    print(cmd, flush=True)
    await bridge.evalInEmacs(cmd)

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
def runGrammarlyDemo():
    hostName = "127.0.0.1"
    webServer = HTTPServer((hostName, port), GrammarlyDemo)
    print("Server started http://%s:%s" % (hostName, port), flush=True)
    try:
        webServer.serve_forever()
    except KeyboardInterrupt:
        pass

# define a handler for chrome received websockets.
async def handleWebSocketFrameReceived(params):
    payload = params["response"]["payloadData"]
    await handleWsMsg(payload)

# check a string if a json.
def isJson(msg):
    try:
        json.loads(msg)
    except ValueError as e:
        return False
    return True


# handle websocket mesage, send a request for add overlays on error position.
async def handleWsMsg(msg):
    if isJson(msg):
        obj = json.loads(msg)
        if "point" in obj:
            category = obj["category"]
            begin = obj["begin"] + 1
            end = obj["end"] + 1
            analyseInfo["infos"].append(obj)
            emacsCmd = '(websocket-bridge-grammarly-overlay-from "{category}" {begin} {end})'.format(
                category=category, begin=begin, end=end
            )
            await runAndLog(emacsCmd)


# using playwright to connect Grammarly demo and login with account.
async def connect_grammarly_account():
    async with async_playwright() as p:
        browser_type = p.chromium
        browser = await browser_type.launch(# headless=False
                                            )
        context = await browser.new_context()
        cookies = chrome_cookies("https://app.grammarly.com/")
        cookie_array = [
            {"name": x, "value": y, "path": "/", "domain": ".grammarly.com"}
            for x, y in cookies.items()
        ]
        await context.add_cookies(cookie_array)
        global page
        page = await context.new_page()
        await page.goto("http://127.0.0.1:" + str(port))

        await page.type("textarea", " ")
        element = await page.wait_for_selector("button")
        await element.click()
        buttons = await page.wait_for_selector("button")
        connect_button = await page.wait_for_selector(
            'text="Connect your Grammarly account"'
        )
        await connect_button.click()
        while len(context.pages) != 2:
            await page.wait_for_timeout(500)
        connect_page = context.pages[1]
        connect_button = await connect_page.wait_for_selector('text="Connect"')
        await connect_button.click()
        f12 = await page.context.new_cdp_session(page)
        await f12.send("Network.enable")
        await f12.send("Page.enable")
        f12.on("Network.webSocketFrameReceived", handleWebSocketFrameReceived)
        await page.wait_for_timeout(300000)

# dispatch message recived from Emacs.
async def messageDispatcher(message):
    info = json.loads(message)
    cmd = info[1][0].strip()
    sentenceStr = info[1][1]
    point = info[1][2]
    if cmd == "analyze":
        await removeOverlays()
        await analyzeSentence(sentenceStr)
    elif cmd == "getInfo":
        await getAnalyzeInfo(point)
    else:
        print("not fount dispatcher", flush=True)
        
        
# send request to Emacs and remove overlays.
async def removeOverlays():
    emacsCmd = '(remove-overlays)'
    await runAndLog(emacsCmd)
    

# get current piont the analyze info.
async def getAnalyzeInfo(point):
    for info in analyseInfo["infos"]:
        if point > info["begin"] and point < info["end"]:
            html = extractHtml(info)
            await runAndLog('(websocket-bridge-grammarly-render "{html}")'.format(html=html))
            break


# extract html from websocket json.
def extractHtml(json):
    title = json["point"].replace('"', "'")
    transforms = json["transforms"]
    details = json["details"].replace('"', "'")
    explanation = json["explanation"].replace('"', "'")
    examples = json["examples"].replace('"', "'")
    transformHtml = ""
    for transform in transforms:
        transform = re.sub(
            r"<span class='gr_grammar_del'>(.*?)<\/span>",
            r"<del>\1</del> -> ",
            transform.replace('"', "'")
        )
        transformHtml += "<p>{transform}</p>".format(transform=transform)
        
    html = """<h1>{title}</h1>
<h2>transforms</h2>
{transformHtml} 
<h2>details</h2>
{details} 
<h2>explanation</h2>
{explanation}
<h2>examples</h2>
{examples}""".format(
        title=title,
        transformHtml=transformHtml,
        details=details,
        explanation=explanation,
        examples=examples,
    )
    return html

# type message in Grammarly Demo to analyze it.
async def analyzeSentence(sentence):
    if analyseInfo["raw"] != sentence:
        await clear(page, "textarea")
        await page.type("textarea", sentence)

# clear page's child dom by selector.
async def clear(page, selector):
    await page.evaluate(
        'selector => document.querySelector(selector).value = ""', selector
    )

# main
async def main():
    t = threading.Thread(target=runGrammarlyDemo)
    t.start()
    global bridge
    bridge = websocket_bridge.WebsocketBridge(
        sys.argv[1], sys.argv[2], messageDispatcher
    )
    await asyncio.gather(bridge.start(), connect_grammarly_account())


asyncio.run(main())
