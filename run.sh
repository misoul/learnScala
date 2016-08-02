#!/bin/bash

APPNAME=CareerCup

scalac -deprecation -d out src/main/scala/com/misoul/${APPNAME}.scala && \
  scala -cp ./out com.misoul.$APPNAME "$@"
