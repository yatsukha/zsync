#!/usr/bin/env bash
sbt assembly && native-image -jar target/scala-*/zsync-assembly*.jar zsync