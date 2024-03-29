import * as Api from './eradio/api.js';
import * as WebSocketUtil from './eradio/websocket.js';
import { Preferences } from './eradio/preferences.js';
import { PlayerUI } from './eradio/ui.js';

export { ERadio };

const METADATA_REFRESH_INTERVAL = 50;

class ERadio {
    playerUI: PlayerUI;
    preferences: Preferences;

    lastMetadataRefresh = 0;
    refreshMetadataTimer: number | null = null;

    constructor() {
        this.playerUI = new PlayerUI();
        this.preferences = new Preferences();
    }

    private get listenerId(): number {
        return this.preferences.listenerId.getOrInsert(() => Math.floor(Math.random() * 4294967296));
    }

    async main() {
        let initialVolume = this.preferences.volume.value ?? this.playerUI.stream.media.volume;
        this.playerUI.volumeSlider.valueAsNumber = initialVolume * 100.0;
        this.playerUI.stream.media.volume = initialVolume;

        this.playerUI.playButton.onclick = _ => this.tryLoadPlayer();
        this.playerUI.vetoButton.onclick = _ => this.veto();
        this.playerUI.volumeSlider.onchange = _ => this.changeVolume();
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

    async handleNotifyWsEvent(event: Event | Error) {
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

    playerStalled() {
        console.log("playback stalled...");
        this.tryLoadPlayer();
    }

    playerSuspended() {
        console.log("playback suspended...");
    }

    tryLoadPlayer() {
        this.playerUI.tryLoad(Api.streamUri("mp3", this.listenerId, Date.now()))
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

    changeVolume() {
        let newVolume = this.playerUI.volumeSlider.valueAsNumber / 100.0;
        this.preferences.volume.value = newVolume;
        this.playerUI.stream.media.volume = newVolume;
    }
}
