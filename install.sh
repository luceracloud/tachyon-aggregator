#! /bin/sh



# ~~~~~~~~~~~~ #
# Dependencies #
# ~~~~~~~~~~~~ #

## git
echo 'Y' | pkgin install scmgit-base

## gcc
# Try to install both versions
echo 'Y' | pkgin install gcc47-4.7.2nb3
echo 'Y' | pkgin install gcc47-4.7.2

## gmake
echo 'Y' | pkgin install gmake

# zmq

# protobuf

# fastbit

