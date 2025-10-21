#!/bin/sh
#
# ./help.sh > HELP.TXT

awk '/^;[+.\/]? ([^(][^ ]*|\()\t/,/^$/ { print }' forth.asm \
      | sed -E '/^;    /d' \
      | sed -E '/^;?$/d' \
      | sed -E 's/^;([+./]? |		)//' \
      | sed -E 's/^([^	]+)[	]+(.*)/=\1 ( \2 )/' \
      | sed -E 's/^;//' \
      | sed -E 's/$//'
