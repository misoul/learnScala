#!/bin/bash

APPNAME=ConsumeStdin

scalac -d target src/com/misoul/main/${APPNAME}.scala && \
  scala -deprecation -cp ./target com.misoul.main.$APPNAME "$@"
