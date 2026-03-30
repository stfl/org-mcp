default:
    @just --list

build:
    eask recompile

test:
    eask run script test

lint:
    ./check.sh

act *args:
    act {{args}}
