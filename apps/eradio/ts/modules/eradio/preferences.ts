export { Preferences };

class Preferences {
    private storage: Storage | undefined;

    volume: Preference<number>;
    listenerId: Preference<number>;

    constructor() {
        try {
            this.storage = window.localStorage;
        } catch (ex) {
            try {
                this.storage = window.sessionStorage;
                console.warn("error accessing local storage; using session storage");
            } catch (ex) {
                console.warn("error accessing session storage; using transient storage");
            }
        }

        this.volume = new Preference(this.storage, 'volume');
        this.listenerId = new Preference(this.storage, 'listener_id');
    }
}

class Preference<T> {
    private key: string;
    private cachedValue: T | undefined;
    private storage: Storage | undefined;

    constructor(storage: Storage | undefined, key: string) {
        this.storage = storage;
        this.key = key;
    }

    get value(): T | undefined {
        if (this.cachedValue !== undefined) {
            return this.cachedValue;
        }

        if (this.storage !== undefined) {
            let valueJson = this.storage.getItem(this.key);
            if (valueJson !== null) {
                let value = JSON.parse(valueJson);
                this.cachedValue = value;
                return value;
            }
        }
    }

    set value(value: T | undefined) {
        this.cachedValue = value;

        if (this.storage !== undefined) {
            if (value !== undefined) {
                this.storage.setItem(this.key, JSON.stringify(value));
            } else {
                this.storage.removeItem(this.key);
            }
        }
    }

    getOrInsert(insert: () => T): T {
        let value = this.value;
        if (value === undefined) {
            value = insert();
            this.value = value;
        }
        return value;
    }
}
