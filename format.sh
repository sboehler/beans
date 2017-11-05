#!/usr/bin/env bash

find src -name "*.hs" -exec hindent {} \;
find app -name "*.hs" -exec hindent {} \;
# find tests -name "*.hs" -exec hindent {} \;
