function brAssets() {
    find assets \( -name '*.css' -o -name '*.svg' -o -name '*.js' \) -exec brotli -f -9 {} \;
}

function gzAssets() {
    find assets \( -name '*.css' -o -name '*.svg' -o -name '*.js' \) -exec gzip -k -f -9 {} \;
}

function compressAssets {
    brAssets
    gzAssets
}
