mkdir $out ${!platf}
cp --recursive $data/pub/3d-mdl ${!platf}
sed 's/^\s*//g' $data/pub/templ.html \
    | sed \
        --expression="/<script>/ r $NIX_BUILD_TOP/bundle.js" \
        --expression="/<style>/ r $NIX_BUILD_TOP/bundle.css" \
    | sed \
        --null-data \
        --expression='s/>\n/>/g' \
        --expression='s/\n</</g' \
    > ${!platf}/index.html
