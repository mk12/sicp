#!/bin/bash

set -eufo pipefail

watch=("$1")

case ${2-} in
    new)
        watch+=(tools/lua/schemehl.c)
        export LUA_CPATH="lib/?.so"
        cmd="make -s lib/schemehl.so DEBUG=1 && echo &&
        pandoc --lua-filter <(echo '
            schemehl = require(\"schemehl\")
            function Code(el)
                local html = (
                    \"<code>\"
                    .. schemehl.highlight(el.text)
                    .. \"</code>\"
                )
                return pandoc.RawInline(\"html\", html)
            end
            function CodeBlock(el)
                local html = (
                    \"<pre><code class=\\\"codeblock\\\">\"
                    .. schemehl.highlight(el.text, {
                         sicp_id_link = function(id)
                            return \"FOO-\" .. id
                         end
                     })
                    .. \"</code></pre>\"
                )
                return pandoc.RawBlock(\"html\", html)
            end
        ') < $1 | bat -pl html; echo"
        ;;
    *)
        cmd="pandoc -dnotes/pandoc/config.yml -Mid=1 $1 -Mtitle=foo \
            | sed -n '/^<main/,/^<\/main/p' | sed '1d;\$d' \
            | sd '<code class=\"sourceCode scheme\">' '<code>' \
            | sd '<div class=\"sourceCode\" id=\"[^\"]+\"><pre class=\"sourceCode scheme\"><code>' \
                '<pre><code class=\"blockcode\">' \
            | sd '<span id=\"[^\"]+\"><a href[^>]+></a>(.*)</span>(?:(</code></pre>)</div>)?' '\$1\$2' \
            | bat -pl html; echo"
esac

printf "%s\n" "${watch[@]}" | SHELL=/bin/bash entr -ccs "$cmd"
