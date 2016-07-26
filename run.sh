#!/bin/bash

APPNAME=DigestLogfile

scalac -d target src/com/misoul/main/${APPNAME}.scala && \
  scala -cp ./target com.misoul.main.$APPNAME 4 345 5634 534 453 543
