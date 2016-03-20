#!/bin/sh
rm -f a.exe
# g++ test.cpp -std=c++0x \
# 	-I../openvr/headers -L../openvr/bin/win64/ -lopenvr_api \
# 	-ISDL2-2.0.4/x86_64-w64-mingw32/include/SDL2 \
# 	-mwindows \
# 	-lmingw32 \
# 	-LSDL2-2.0.4/x86_64-w64-mingw32/lib -lSDL2main -lSDL2 -v

g++ test.cpp -std=c++0x \
	-I../openvr/headers -L../openvr/bin/win64/ -lopenvr_api \
	-I/usr/include/SDL2 \
	-L/usr/lib -lmingw32 -lSDL2main -lSDL2
	#`sdl2-config --libs`
./a.exe