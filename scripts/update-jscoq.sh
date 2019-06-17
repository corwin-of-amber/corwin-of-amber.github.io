#!/bin/bash

here=`pwd`
there=~/var/ext/jscoq

cd $there

rm -rf _build/dist
make dist

cd $here
rm -rf jscoq
mv $there/_build/dist ./jscoq
#( cd jscoq && npm i )

# -- Personal Customization --

# Replace index with gentle-intro
# (crazy regex! transforms '../ -> './ and "../ -> "./)
sed 's/\(["'\'']\)\.\(\.\/\)/\1\2/' jscoq/examples/gentle-intro.html > jscoq/index.html
