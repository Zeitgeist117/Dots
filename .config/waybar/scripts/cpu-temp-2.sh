#!/bin/sh
ctmp=$(sensors | awk '/Core 0/ {print$3}' | sed 's/+//')
echo $ctmp
