class HTTPStatusError extends Error {
    constructor(response) {
        super("HTTP returned " + response.status);
        this.status = response.status;
    }
}
export async function metadata() {
    let response = await apiFetch("/v1/metadata");
    return await response.json();
}
export async function veto(listenerId) {
    await apiFetch("/v1/veto?listener_id=" + listenerId, { method: "PUT" });
}
export function notifyWsUri() {
    return "/v1/ws/notify";
}
export function streamUri(_format, listenerId, timeMs) {
    return "/stream.mp3?listener_id=" + listenerId + "&time=" + timeMs;
}
async function apiFetch(resource, init) {
    let response = await fetch(resource, init);
    if (response.ok) {
        return response;
    }
    else {
        throw new HTTPStatusError(response);
    }
}
