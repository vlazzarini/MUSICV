#!/bin/sh
# release script
# ./release.sh platform-version
cd ..
mkdir release-$1
cd release-$1
cmake .. -DCMAKE_INSTALL_PREFIX=.
make install
mv bin music5-$1
tar zcf music5-$1.tgz music5-$1
