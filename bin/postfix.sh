#!/bin/bash
#
# Convenience PostFix tester. Just run it and type:
#
#   > postfix "3 14 mul"
#
# Author: Martin Krauskopf <martin.krauskopf at gmail.com>
#

set -o nounset # Treat unset variables as an error
set -e

cd "$PWD/`dirname $0`/.."
ghci -isrc src/DCPL/PostFixEval.hs

