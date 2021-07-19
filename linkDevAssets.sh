OUT="./webdev"
ARTIFACT="./dist-ghcjs/build/x86_64-linux/ghcjs-8.6.0.1/chopaan-0.1.0.0/x/ui/build/ui/ui.jsexe"

jslib="./js"
assets="./assets"

if [ -d $OUT ]; then
    echo "Deleting ${OUT}..."
    rm -r $OUT
fi

echo "Recreating ${OUT}"
mkdir -p $OUT
mkdir -p $OUT/assets
mkdir -p $OUT/js

echo "Linking JS sources"
for src in ${jslib}/*
do
    ln -s $src $OUT/js/$(basename $src)
done

echo "Linking Artifacts"
for asset in ${assets}/*
do
    ln -s $asset $OUT/assets/$(basename $asset)
done

if [ ! -d $ARTIFACT ]; then
    echo "JS Executable Not Found! Please run: `make devjs` "
    exit
fi
cp ${ARTIFACT}/all.js $OUT/all.min.js
cp ${ARTIFACT}/index.html $OUT/index.html
echo $(git log --pretty=format:'%h' -n 1) > $OUT/version
echo "Asset Linking Finished!"
