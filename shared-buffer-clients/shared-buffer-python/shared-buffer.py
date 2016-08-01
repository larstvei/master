from sys import argv
from uuid import uuid4
from random import choice, gauss
import websocket
import thread
import codecs
import json
import time

key = None
seqno = 0
token = 0
buffer = ''

def applyops(operations):
    global buffer

    for op in operations:
        pos = op['pos']
        if op.has_key('ins'):
            buffer = buffer[0:pos] + op['ins'] + buffer[pos:]
        elif op.has_key('del'):
            buffer = buffer[0:pos] + buffer[pos + len(op['del']):]

def on_message(ws, message):
    global key
    global token
    global buffer

    msg = json.loads(message)

    if msg['type'] == 'connect-response':
        key = msg['session']
    elif msg['type'] == 'buffer-request':
        ws.send(json.dumps({'type'  : 'buffer-response',
                            'session' : key, 'token' : token,
                            'operation' : {'pos' : 0, 'ins' : buffer}}))
    elif msg['type'] == 'operations':
        key = msg['session']
        on_operation(msg)

def on_operation(msg):
    global seqno
    global token

    if msg['seqno'] == seqno:
        token = msg['token']
        applyops(msg['operations'])
    seqno += 1

def on_change(ws, str, pos, type):
    global key
    global seqno
    global token

    op = { type : str, 'pos' : pos }
    msg = {'type' : 'operation', 'session' : key,
           'seqno' : seqno, 'token' : token, 'operation' : op}

    applyops([op])
    ws.send(json.dumps(msg))
    seqno += 1

def on_error(ws, error):
    print error

def on_close(ws):
    print "### closed ###"

def gen_message(key):
    global seqno
    global token
    global buffer

    type = choice(['ins', 'ins', 'del']) if len(buffer) != 0 else 'ins'
    pos = choice(range(len(buffer) + 1)) if type == 'ins' else choice(range(len(buffer)))
    str = choice('abcdefghijklmnopqrstuvwxyz \n') if type == 'ins' else buffer[pos]
    return type, pos, str

def opener(keyreq, secs):
    def on_open(ws):
        ws.send(json.dumps({'type' : 'connect-request', 'session' : keyreq}))

    def run(*args):
        global key
        global seqno
        global buffer

        t = time.time()

        while time.time() - t < secs:
            if key == None:
                continue
            time.sleep(max(0.01, gauss(0.3, 0.8)))
            type, pos, s = gen_message(key)
            print type, pos, s
            on_change(ws, s, pos, type)

        time.sleep(10)

        with codecs.open('sim_results/' + key + '-' + str(uuid4()), 'w', 'utf-8') as f:
            f.write(buffer + '\n')

        ws.close()
    thread.start_new_thread(run, ())
    return on_open

if __name__ == "__main__":
    ws = websocket.WebSocketApp("ws://localhost:3705", # "ws://52.50.227.217:3705",
                                on_message = on_message,
                                on_error = on_error,
                                on_close = on_close)
    secs = 10

    if len(argv) > 2:
        secs = argv[2]

    ws.on_open = opener(argv[1] if len(argv) > 1 else None, secs)

    ws.run_forever()
