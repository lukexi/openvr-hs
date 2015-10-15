# OpenVR-HS
Bindings for [OpenVR](https://github.com/ValveSoftware/openvr) and the [Valve/HTC Vive](http://store.steampowered.com/universe/vr)

These are preliminary but working quite smoothly.

See https://github.com/lukexi/openvr-hs/blob/master/app/Main.hs for basic example usage.

You'll need a few dependencies until I release the bindings on Hackage:
```
git clone --recursive git@github.com:lukexi/openvr-hs.git
git clone git@github.com:lukexi/glfw-pal.git
git clone git@github.com:lukexi/gl-pal.git
git clone git@github.com:lukexi/linear-extra.git
git clone git@github.com:lukexi/lens-extra.git
git clone git@github.com:lukexi/halive.git
```

The build system expects a 64bit [MSYS2](https://msys2.github.io) installation.

Note: you may also need this version of GHC until 7.10.3 comes out to fix a small linker bug.
https://github.com/lukexi/ghc/releases/download/ghc-7.10.2-release-plus-rework-windows-pe-linker/ghc-7.10.2-x86_64-unknown-mingw32-windows-linker-fixes.tar.bz2
