#!/bin/bash
rm -rf dist
mkdir -p dist
elm make src/Main.elm --output dist/nedtelling.js
cp static/* dist/
