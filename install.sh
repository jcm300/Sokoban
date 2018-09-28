#!/bin/bash
mkdir build
stack setup
stack build --copy-bins --local-bin-path build
rm -r .stack-work
cp -r ./src/images/ build
cp -r ./src/levels/ build
