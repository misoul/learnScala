#!/bin/bash

APPNAME=Sort

scalac -d target src/com/misoul/main/${APPNAME}.scala && \
  scala -cp ./target com.misoul.main.$APPNAME 4 345 534 453 543 s as dsa s23 324s asd  34 ds
