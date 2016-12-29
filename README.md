**N.B.:** This is one of those projects where you need to read the instructions, a simple `cabal run` won't work, apologies!

# What

This project demonstrates how to write a HTML application in Haskell via the webkit2 embedding API.

This code does *not* run a webserver. It uses the GObject introspection API to access the DOM directly.

# Quick start

```bash
git clone https://github.com/teh/haskell-webkit2gtk
nix-shell  # stackage: not tried yet if anyone wants to pick that up
cabal build gluon
cabal build plugin
dist/build/gluon/gluon --plugin dist/build/plugin/plugin
```

# SCSS resources

All scss resources are injected as rendered CSS into `<head>`. Pass them on the command line:

```bash
dist/build/gluon/gluon --scss /home/tom/src/bootstrap/scss/bootstrap.scss --plugin dist/build/plugin/plugin
```
