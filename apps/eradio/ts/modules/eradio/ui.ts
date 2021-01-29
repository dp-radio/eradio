import { Metadata } from './api.js';

export { PlayerUI };

class PlayerUI {
    playButton: HTMLElement;
    vetoButton: HTMLElement;
    stream: StreamingMediaUI;
    status: StatusUI;
    metadata: MetadataUI;

    constructor() {
        this.playButton = element('#eradio_play_button', HTMLElement);
        this.vetoButton = element('#eradio_veto_button', HTMLElement);
        this.stream = new StreamingMediaUI('#eradio_player');
        this.status = new StatusUI('#eradio_status');
        this.metadata = new MetadataUI('#eradio_metadata');
    }

    refresh(metadata: Metadata) {
        this.metadata.refresh(metadata);
    }

    refreshPlayerStatus() {
        let playerLag = this.stream.lag();
        if (playerLag !== null) {
            this.status.setLag(playerLag);
        } else {
            this.status.stop();
        }
    }

    tryLoad(url: string) {
        if (this.stream.tryLoad(url)) {
            this.refreshPlayerStatus();
        }
    }
}

class StreamingMediaUI {
    media: HTMLMediaElement;
    source: HTMLSourceElement;

    lastPlayerLoad = 0;

    constructor(selector: string) {
        this.media = element(selector, HTMLMediaElement);
        this.source = subelement(this.media, "source", HTMLSourceElement);
    }

    tryLoad(url: string): boolean {
        let now = Date.now();
        let duration = Math.abs(now - this.lastPlayerLoad);
        if (duration > 2000) {
            console.log("reloading player");
            this.source.src = url;
            this.media.load();
            this.media.play();
            this.media.currentTime = 10;
            this.lastPlayerLoad = Date.now();
            return true;
        } else {
            console.log("inhibiting player reload after only "+duration+"ms");
            return false;
        }
    }

    lag() {
        let lag = null;
        if (this.media.buffered.length != 0) {
            let bufferedLag = (this.media.buffered.end(this.media.buffered.length - 1) - this.media.currentTime);
            lag = Math.max(lag || 0, bufferedLag);
        }
        if (this.media.seekable.length != 0) {
            let seekableEnd = this.media.seekable.end(this.media.seekable.length - 1);
            if (isFinite(seekableEnd)) {
                let seekableLag = (seekableEnd - this.media.currentTime);
                lag = Math.max(lag || 0, seekableLag);
            }
        }
        return lag;
    }
}

class StatusUI {
    status: HTMLElement;
    stopped: HTMLElement;
    loading: HTMLElement;
    loadingCycle: HTMLElement[];
    playing: HTMLElement;
    playingLag: HTMLElement;
    playingLagValue: HTMLElement;

    constructor(selector: string) {
        this.status = element(selector, HTMLElement);
        this.stopped = subelement(this.status, ".eradio_status_stopped", HTMLElement);
        this.loading = subelement(this.status, ".eradio_status_loading", HTMLElement);
        this.loadingCycle = subelements(this.loading, ".eradio_status_loading_cycle", HTMLElement);
        this.playing = subelement(this.status, ".eradio_status_playing", HTMLElement);
        this.playingLag = subelement(this.playing, ".eradio_status_playing_lag", HTMLElement);
        this.playingLagValue = subelement(this.playingLag, ".eradio_status_playing_lag_value", HTMLElement);
    }

    setLag(lag: number) {
        if (lag > 3) {
            this.playingLag.style.display = "";
            this.playingLagValue.textContent = lag.toLocaleString([], {maximumFractionDigits: 2});
        } else {
            this.playingLag.style.display = "none";
        }

        this.stopped.style.display = "none";
        this.loading.style.display = "none";
        this.playing.style.display = "";
    }

    stop() {
        for (const loadingCycleElement of this.loadingCycle) {
            loadingCycleElement.style.display = "none";
        }
        this.loadingCycle[Math.floor(Math.random() * this.loadingCycle.length)].style.display = "";

        this.stopped.style.display = "none";
        this.playing.style.display = "none";
        this.loading.style.display = "";
    }
}

class MetadataUI {
    metadata: HTMLElement;
    metadataSong: HTMLAnchorElement;
    metadataNoSong: HTMLElement;
    listenerCount: HTMLElement;
    vetoCount: HTMLElement;

    constructor(selector: string) {
        this.metadata = element(selector, HTMLElement);
        this.metadataSong = subelement(this.metadata, '.eradio_metadata_song', HTMLAnchorElement);
        this.metadataNoSong = subelement(this.metadata, '.eradio_metadata_no_song', HTMLElement);
        this.listenerCount = element('#eradio_listener_count', HTMLElement);
        this.vetoCount = element('#eradio_veto_count', HTMLElement);
    }

    refresh(metadata: Metadata) {
        if (metadata != null) {
            if (metadata.current_track != null) {
                this.metadataSong.href = metadata.current_track.uri;
                this.metadataSong.target = "_blank";
                this.metadataSong.textContent = metadata.current_track.name;

                this.metadataNoSong.style.display = "none";
                this.metadataSong.style.display = "";
            } else {
                this.metadataSong.style.display = "none";
                this.metadataNoSong.style.display = "";
            }

            this.listenerCount.textContent = "" + metadata.listener_count;
            this.vetoCount.textContent = "" + metadata.veto_count;
        }
    }
}

class MissingUIElementError extends Error {
    selector: string;
    constructor(selector: string) {
        super("missing element: " + selector);
        this.selector = selector;
    }
}

class UIElementTypeError extends Error {
    selector: string;
    constructor(selector: string) {
        super("wrong element type: " + selector);
        this.selector = selector;
    }
}

function element<T>(selector: string, cls: { new (): T }): T {
    return subelement(document, selector, cls);
}
function subelement<T>(parent: Document | Element, selector: string, cls: { new (): T }): T {
    let elements = subelements(parent, selector, cls);
    if (elements.length == 0) {
        throw new MissingUIElementError(selector);
    }
    return elements[0];
}
function subelements<T>(parent: Document | Element, selector: string, cls: { new (): T }): T[] {
    const nodeList = parent.querySelectorAll(selector);
    let elements = [];
    for (const element of nodeList.values()) {
        if (!(element instanceof cls)) {
            throw new UIElementTypeError(selector);
        }
        elements.push(element);
    }
    return elements;
}
