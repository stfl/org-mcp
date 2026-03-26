default:
    @just --list

build:
    eask recompile

tests:
    eask run script test

lint:
    ./check.sh

act *args:
    act {{args}}
