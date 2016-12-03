This code demonstrates how to write a HTML application in Haskell via the webkit2 embedding API.

This code does *not* run a webserver. It uses the GObject introspection API to access the DOM directly.

# Quick start

```bash
git clone https://github.com/teh/haskell-webkit2gtk
nix-shell  # stackage: not tried yet if anyone wants to pick that up
cabal build web
cabal build plugin && cp dist/build/plugin/plugin ext/test.so
dist/build/web/web
```

# SCSS resources

All scss resources are injected as rendered CSS into `<head>`. Pass them on the command line:

```bash
dist/build/web/web  --scss /home/tom/src/bootstrap/scss/bootstrap.scss
```
