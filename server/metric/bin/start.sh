#!/usr/bin/env bash
BASE=/opt/meter

LD_LIBRARY_PATH=$BASE/lib $BASE/bin/server_release $1
