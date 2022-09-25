-module(eradio_child).

-callback child_spec() -> supervisor:child_spec().
