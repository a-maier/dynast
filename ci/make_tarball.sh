#$/bin/sh

set -ex

mkdir -p $PROJECT $PROJECT/bin $PROJECT/lib
cp target/$1/release/$PROJECT $PROJECT/bin/
cp target/$1/release/lib$PROJECT.* $PROJECT/lib/
cp -r share $PROJECT/
tar czf $PROJECT.tar.gz $PROJECT
rm -r $PROJECT
