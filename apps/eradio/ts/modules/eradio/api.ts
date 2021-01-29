export { Metadata };

interface Metadata {
    current_track: Track;
    listener_count: number;
    veto_count: number;
}

interface Track {
    name: string;
    uri: string;
}

type StreamFormat = 'mp3';

class HTTPStatusError extends Error {
    status: number;
    constructor(response: Response) {
        super("HTTP returned " + response.status);
        this.status = response.status;
    }
}

export async function metadata(): Promise<Metadata> {
    let response = await apiFetch("/v1/metadata");
    return await response.json() as Metadata;
}

export async function veto(listenerId: number) {
    await apiFetch("/v1/veto?listener_id="+listenerId, {method: "PUT"});
}

export function streamPath(_format: StreamFormat, listenerId: number, timeMs: number) {
    return "/stream.mp3?listener_id="+listenerId+"&time="+timeMs;
}

interface FetchOptions {
    method?: 'PUT';
}

async function apiFetch(resource: string | Request, init?: FetchOptions): Promise<Response> {
    let response = await fetch(resource, init);
    if (response.ok) {
        return response;
    } else {
        throw new HTTPStatusError(response);
    }
}
