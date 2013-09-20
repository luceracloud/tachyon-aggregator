#!/usr/bin/env bash
BASE=/opt/meter

LD_LIBRARY_PATH=$BASE/lib $BASE/bin/generator $1 -n `uname -n | sed 's/\..*//'`
