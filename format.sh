#!/usr/bin/env bash

find src -name "*.hs" -exec brittany --write-mode inplace {} \;
find app -name "*.hs" -exec brittany --write-mode inplace {} \;
find tests -name "*.hs" -exec brittany --write-mode inplace {} \;
