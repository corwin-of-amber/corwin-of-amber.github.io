BEGINFILE { has=0 }
/jscoq/ { has=1 }
/<[/]html>/ && has<1 {
    print "<script src=\"https://jscoq.github.io/node_modules/jscoq/ui-js/jscoq-loader.js\" type=\"text/javascript\"></script>"
    print "<script src=\"../js/jscoq-embed.js\" type=\"text/javascript\"></script>"
}
{ print }
