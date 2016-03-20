
#LIBDIR=openvr/bin/win64/
#LIBDIR=/d/Steam/steamapps/common/SteamVR/bin/win64/

g++ testCPP.cpp -std=c++0x -I../openvr/headers -L../openvr/bin/win64/ -lopenvr_api