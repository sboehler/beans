#!/usr/bin/env bash

find src -name "*.hs" -exec ormolu --mode=inplace {} \;
find app -name "*.hs" -exec ormolu --mode=inplace {} \;
