mkdir $out
for jsLibDep in $jsLibDeps; do
    cp --recursive $jsLibDep/lib/node_modules/* $out
done
find $data/js-lib-used/* | xargs cat > $out/exps.js
