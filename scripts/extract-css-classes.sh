# bulma css bundle is too big
# purge css tool should help
# it requires HTML content or list of classes to be preserved
# the content is dynamic so it is difficult save all HTML pages
# the script tries just extract classes with regex

function extractCssClasses() {
    { grep -rhPo '(?<=class=)[^"][^> ]+' src/PieceOfFlake ;
      grep -Po '((?<=[(]["])[a-z-]+|(?<=className = ")[^"]+)' assets/app.js
      grep -rhPo '(?<=class=["])[^"#]+' src/PieceOfFlake | grep -oE "[^ ]+" ;
    }  | sort -u | sed 's/^/-s /'
}

# ./node_modules/.bin/purgecss -css bulma.min.css -v  -o out
# npm install purgecss --save-dev
#
function stripCss() {
    rm -rf stripped
    mkdir stripped
    touch stripped/index.html
    ./node_modules/.bin/purgecss -css assets/bulma.min.css -con stripped/index.html \
                                 -v -s span \
                                 $(extractCssClasses) -o stripped
    cp stripped/bulma.min.css assets/bulma.min.css
    rm -rf stripped
}
