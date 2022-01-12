Vlojure is a visual programming interface for ClojureScript. You can use vlojure by visiting [vlojure.io](https://vlojure.io) in your browser. See [vlojure.io/about](https://vlojure.io/about)


# Local Development
To work on vlojure, clone the repo and run `npm i` to install the npm dependencies.

## Live
To use [Shadow CLJS](https://github.com/thheller/shadow-cljs)'s "watch" mode while developing, which first run `clojure -M:server` in the root directory. This starts up local [stasis](https://github.com/magnars/stasis) development server on `localhost:3000`. Then, run `npm run watch` in the root directory to start  in watch mode.

## Exporting
To export an optimized release build,  run `npm run release && clojure -M:export`. This will create an "out" directory that contains the exported static site.


# TODO
* **Optimize**. Right now the app essentially re-renders everything each frame. Instead, elements should only be redrawn when the app state has changed in a relevant way.
* **Fix mobile UI issues.** Vlojure is intended in large part for use on touch-screen mobile devices, but right now there are some issues that impact usability on mobile. Known issues include:
  * App does not respond properly to the presence of virtual keyboards
  * On iOS (or at least iPad), html  text input elements are not large enough vertically. They seem to be roughly half the height they should be.
  * On android, when tapping to edit a sufficiently small literal, the page automatically zooms in in a way that disrupts the normal view of the app. It does not go back to normal once the user finishes editing the literal.
* Make **native mobile apps** for android and iOS.
bindings at the start of the bar, and broader-scoped bindings later in the bar.
* **Prevent the app from hanging** when the user tries to evaluate an infinite sequence, or otherwise gets stuck in an infinite loop.
* **Give the user more feedback about errors**. Right now when an evaluation ends in an error, the user just sees a big "X" in the eval zone. Instead, they should recieve more detail about the nature of the error.
