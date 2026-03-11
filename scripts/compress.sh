function brAssets() {
    find assets \( -name '*.css' -o -name '*.svg' -o -name '*.js' \) -exec brotli -9 {} \;
}

function gzAssets() {
    find assets \( -name '*.css' -o -name '*.svg' -o -name '*.js' \) -exec gzip -k -9 {} \;
}

function compressAssets {
    brAssets
    gzAssets
}
