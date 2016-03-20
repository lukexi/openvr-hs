#!/bin/sh
rm -f a.exe

gcc testCFnTable.c -I../openvr/headers -L../openvr/bin/win64/ -lopenvr_api

./a.exe