ghcjs \
    -D=${platf^^}_PLATF \
    -Wall \
    -hidir=$NIX_BUILD_TOP \
    -i=$comm \
    -i=$src \
    -o $NIX_BUILD_TOP/$name \
    -odir=$NIX_BUILD_TOP \
    Main
closure-compiler \
    --compilation_level=ADVANCED_OPTIMIZATIONS \
    --dependency_mode=PRUNE \
    --entry_point=$NIX_BUILD_TOP/$name.jsexe/all.js \
    --entry_point=$jsLibDep/exps.js \
    --js_output_file=$NIX_BUILD_TOP/bundle.js \
    --jscomp_off="*" \
    --language_in=ES_NEXT \
    --warning_level=QUIET \
    $NIX_BUILD_TOP/$name.jsexe/all.js \
    $jsLibDep
sass \
    --no-source-map \
    --style=compressed \
    $src/Dtl/View/Comm/Root/${platf^}.sass \
    $NIX_BUILD_TOP/bundle.css
