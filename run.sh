#!/bin/bash

APPNAME=TopReachableLeads

# scalac -deprecation -d out src/main/scala/com/misoul/${APPNAME}.scala && \
#   scala -cp ./out com.misoul.$APPNAME "$@"

scalac -deprecation -cp ~/.ivy2/cache -d out src/main/scala/com/interviews/radius/${APPNAME}.scala && \
  scala -cp ./out:~/.ivy2/cache com.interviews.radius.$APPNAME "$@"
