###Clojuresript chess board using Reagent

#####Steps
A. Test Mode
  1. `rlwrap lein figwheel`

B. Production mode
  1. `lein clean`
  2. `lein cljsbuild once prod`

C. Finally
  View `resources/public/index.html` in your browser

To note: chess moves aren't verified, you can move anywhere, this is just a glorified drag'n  drop exercise (for now)

The intent was to use HTML5 native drag and drop API, which mostly works (as can be seen if core.cljs.bak is put to use),

but had to resort to jQuery because of pesky ghost image problem. Suggestions welcome.

Anyway, work in progress.
