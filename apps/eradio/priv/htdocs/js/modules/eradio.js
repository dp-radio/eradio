import * as Api from './eradio/api.js';
import { PlayerUI } from './eradio/ui.js';
export { ERadio };
const METADATA_REFRESH_INTERVAL = 1000;
class ERadio {
    constructor() {
        this.lastMetadataRefresh = 0;
        this.refreshMetadataTimer = null;
        this.listenerId = Math.floor(Math.random() * 4294967296);
        this.playerUI = new PlayerUI();
    }
    async main() {
        await this.refreshMetadata();
        this.playerUI.playButton.onclick = _ => this.tryLoadPlayer();
        this.playerUI.vetoButton.onclick = _ => this.veto();
        this.playerUI.stream.media.onstalled = _ => this.playerStalled();
        this.playerUI.stream.media.onsuspend = _ => this.playerSuspended();
        this.playerUI.stream.media.ontimeupdate = _ => this.playerUI.refreshPlayerStatus();
    }
    playerStalled() {
        console.log("playback stalled...");
        this.tryLoadPlayer();
    }
    playerSuspended() {
        console.log("playback suspended...");
    }
    tryLoadPlayer() {
        this.playerUI.tryLoad(Api.streamPath("mp3", this.listenerId, Date.now()));
    }
    scheduleRefreshMetadata() {
        const elapsed = Math.abs(Date.now() - this.lastMetadataRefresh);
        if (elapsed > METADATA_REFRESH_INTERVAL) {
            this.refreshMetadata();
        }
        else if (this.refreshMetadataTimer === null) {
            this.refreshMetadataTimer = setTimeout(() => this.refreshMetadata(), METADATA_REFRESH_INTERVAL - elapsed);
        }
    }
    async refreshMetadata() {
        let metadata = null;
        try {
            metadata = await Api.metadata();
        }
        catch (error) {
            console.log("error fetching metadata: " + error);
        }
        if (metadata != null) {
            this.playerUI.refresh(metadata);
        }
        if (this.refreshMetadataTimer != null) {
            clearTimeout(this.refreshMetadataTimer);
            this.refreshMetadataTimer = null;
        }
        this.lastMetadataRefresh = Date.now();
        this.scheduleRefreshMetadata();
    }
    async veto() {
        await Api.veto(this.listenerId);
    }
}
