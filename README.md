Vlojure is a visual programming interface for ClojureScript. You can use vlojure by visiting [vlojure.io](https://vlojure.io) in your browser. See [vlojure.io/about](https://vlojure.io/about)


# Local Development
To work on vlojure, clone the repo and run `npm i` to install the npm dependencies.

## Live
To use [Shadow CLJS](https://github.com/thheller/shadow-cljs)'s "watch" mode while developing, which first run `clojure -M:server` in the root directory. This starts up local [stasis](https://github.com/magnars/stasis) development server on `localhost:3000`. Then, run `npm run watch` in the root directory to start  in watch mode.

## Exporting
To export an optimized release build,  run `npm run release && clojure -M:export`. This will create an "out" directory that contains the exported static site.
