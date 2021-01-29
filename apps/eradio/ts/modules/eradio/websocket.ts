const RECONNECT_DELAY = 3000;

export async function connectForever(path: string, connectCallback: (ws: WebSocket) => void, handler: (e: Event) => void): Promise<WebSocket> {
    while (true) {
        let websocket;
        try {
            websocket = await connect(path, handler);
        } catch (ex) {
            console.debug('error connecting to websocket '+path+': ', ex);
        }

        if (websocket != null) {
            try {
                connectCallback(websocket);
            } catch (ex) {
                console.error('error in websocket '+path+' connect callback: ', ex);
            }

            let error;
            try {
                error = await join(websocket);
            } catch (ex) {
                error = ex;
            }

            handler(error);
        }

        await sleep(Math.floor(RECONNECT_DELAY * (1 + Math.random())));
    }
}

export async function connect(path: string, handler: (e: Event) => void): Promise<WebSocket> {
    const ws = open(path);
    return new Promise((resolve, reject) => {
        ws.onopen = function (_event) {
            ws.onmessage = handler;
            ws.onclose = handler;
            ws.onerror = handler;
            ping(ws);
            resolve(ws);
        };
        ws.onclose = _event => { reject(); };
        ws.onerror = event => { reject(event); };
    });
}

export async function join(ws: WebSocket): Promise<CloseEvent> {
    return new Promise((resolve, reject) => {
        ws.onclose = event => { resolve(event); };
        ws.onerror = event => { reject(event); };
    });
}

function open(path: string) {
    let location = window.location;
    let scheme;
    if (location.protocol === "https:") {
        scheme = "wss:";
    } else {
        scheme = "ws:";
    }
    let host = location.host;
    return new WebSocket(scheme + "//" + host + path);
}

function ping(ws: WebSocket) {
    if (ws.readyState == WebSocket.OPEN) {
        ws.send('');
    }
    if (ws.readyState != WebSocket.CLOSED) {
        setTimeout(() => ping(ws), 1000);
    }
}

async function sleep(delay_ms: number): Promise<void> {
    return new Promise((resolve, _reject) => {
        setTimeout(() => resolve(), delay_ms);
    });
}
