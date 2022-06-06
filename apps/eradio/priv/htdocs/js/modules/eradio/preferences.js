export { Preferences };
class Preferences {
    constructor() {
        try {
            this.storage = window.localStorage;
        }
        catch (ex) {
            try {
                this.storage = window.sessionStorage;
                console.warn("error accessing local storage; using session storage");
            }
            catch (ex) {
                console.warn("error accessing session storage; using transient storage");
            }
        }
        this.volume = new Preference(this.storage, 'volume');
        this.listenerId = new Preference(this.storage, 'listener_id');
    }
}
class Preference {
    constructor(storage, key) {
        this.storage = storage;
        this.key = key;
    }
    get value() {
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
    set value(value) {
        this.cachedValue = value;
        if (this.storage !== undefined) {
            if (value !== undefined) {
                this.storage.setItem(this.key, JSON.stringify(value));
            }
            else {
                this.storage.removeItem(this.key);
            }
        }
    }
    getOrInsert(insert) {
        let value = this.value;
        if (value === undefined) {
            value = insert();
            this.value = value;
        }
        return value;
    }
}
