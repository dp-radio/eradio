import * as Api from './eradio/api.js';
import * as WebSocketUtil from './eradio/websocket.js';

import { PlayerUI } from './eradio/ui.js';

export { ERadio };

const METADATA_REFRESH_INTERVAL = 50;

class ERadio {
    playerUI: PlayerUI;

    mediaSource: MediaSource | null = null;
    sourceBuffer: SourceBuffer | null = null;

    listenerId: number;

    lastMetadataRefresh = 0;
    refreshMetadataTimer: number | null = null;

    constructor() {
        this.listenerId = Math.floor(Math.random() * 4294967296);
        this.playerUI = new PlayerUI();
    }

    async main() {
        this.playerUI.playButton.onclick = _ => this.tryLoadPlayer();
        this.playerUI.vetoButton.onclick = _ => this.veto();
        this.playerUI.stream.media.onstalled = _ => this.playerStalled();
        this.playerUI.stream.media.onsuspend = _ => this.playerSuspended();
        this.playerUI.stream.media.ontimeupdate = _ => this.playerUI.refreshPlayerStatus();

        this.refreshMetadata();
        WebSocketUtil.connectForever(Api.notifyWsUri(),
                                     ws => this.handleNotifyWsConnected(ws),
                                     event => this.handleNotifyWsEvent(event));
    }

    handleNotifyWsConnected(_ws: WebSocket) {
        console.debug('connected to notify websocket');
        this.refreshMetadata();
    }

    async handleNotifyWsEvent(event: Event) {
        if (event instanceof MessageEvent) {
            await this.handleNotifyWsMessage(event.data);
        } else if (event instanceof CloseEvent) {
            console.debug('notify websocket closed for reason ' + event.code + ': ' + event.reason);
        } else {
            console.debug('error on notify websocket: ', event);
        }
    }

    async handleNotifyWsMessage(message: any) {
        if (message instanceof Blob) {
            message = await message.text();
        }
        if (message === '') {
            return;
        }
        message = JSON.parse(message);
        if (message.action == "notify") {
            this.refreshMetadata();
        } else {
            console.warn("unknown notify websocket message: ", message);
        }
    }

    handleStreamWsConnected(_ws: WebSocket) {
        console.debug('connected to stream websocket');
    }

    async handleStreamWsEvent(event: Event) {
        if (event instanceof MessageEvent) {
            let data = null;
            if (event.data instanceof Blob) {
                data = await event.data.arrayBuffer();
            } else if (event.data instanceof ArrayBuffer) {
                data = event.data;
            }
            if (data != null && this.sourceBuffer != null) {
                this.sourceBuffer.appendBuffer(data);
            }
        } else if (event instanceof CloseEvent) {
            console.debug('stream websocket closed for reason ' + event.code + ': ' + event.reason);
        } else {
            console.debug('error on stream websocket: ', event);
        }
    }

    playerStalled() {
        console.log("playback stalled...");
        this.tryLoadPlayer();
    }

    playerSuspended() {
        console.log("playback suspended...");
    }

    mediaSourceOpened(mediaSource: MediaSource) {
        this.sourceBuffer = mediaSource.addSourceBuffer("audio/mpeg");
    }

    tryLoadPlayer() {
        let url;
        if ('MediaSource' in window && MediaSource.isTypeSupported("audio/mpeg")) {
            let mediaSource = new MediaSource();
            this.mediaSource = mediaSource;
            url = URL.createObjectURL(mediaSource);
            mediaSource.addEventListener('sourceopen', _event => this.mediaSourceOpened(mediaSource));
            WebSocketUtil.connectForever(Api.streamWsUri("mp3", this.listenerId), ws => this.handleStreamWsConnected(ws), event => this.handleStreamWsEvent(event));
        } else {
            url = Api.streamUri("mp3", this.listenerId, Date.now());
        }
        this.playerUI.tryLoad(url);
    }

    scheduleRefreshMetadata() {
        const elapsed = Math.abs(Date.now() - this.lastMetadataRefresh);
        if (elapsed > METADATA_REFRESH_INTERVAL) {
            this.refreshMetadata();
        } else if (this.refreshMetadataTimer === null) {
            this.refreshMetadataTimer = setTimeout(() => this.refreshMetadata(), METADATA_REFRESH_INTERVAL - elapsed);
        }
    }

    async refreshMetadata() {
        let metadata = null;
        try {
            metadata = await Api.metadata();
        } catch(error) {
            console.log("error fetching metadata: "+error);
        }
        if (metadata != null) {
            this.playerUI.refresh(metadata);
        }

        if (this.refreshMetadataTimer != null) {
            clearTimeout(this.refreshMetadataTimer);
            this.refreshMetadataTimer = null;
        }
        this.lastMetadataRefresh = Date.now();
    }

    async veto() {
        await Api.veto(this.listenerId);
    }
}
