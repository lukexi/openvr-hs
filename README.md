# OpenVR-HS
Bindings for [OpenVR](https://github.com/ValveSoftware/openvr) and the [Valve/HTC Vive](http://store.steampowered.com/universe/vr)

These are preliminary but working quite smoothly.

See https://github.com/lukexi/openvr-hs/blob/master/app/Main.hs for basic example usage.


The example has optional hooks for [Halive](https://github.com/lukexi/halive) so you can make live edits to your code.

You'll need a few dependencies until I release the bindings on Hackage:
```
git clone --recursive git@github.com:lukexi/openvr-hs.git
git clone git@github.com:lukexi/glfw-pal.git
git clone git@github.com:lukexi/gl-pal.git
git clone git@github.com:lukexi/linear-extra.git
git clone git@github.com:lukexi/lens-extra.git
git clone git@github.com:lukexi/halive.git
```

Run the demo with `stack test :cubes` or `cabal test cubes`.

The build system expects a 64bit [MSYS2](https://msys2.github.io) installation.

You must use GHC 7.10.3 or greater.

Feel free to contact me via the issues tab or [@lukexi](http://twitter.com/lukexi) if you need any help getting set up.
