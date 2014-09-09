(defproject nintynine "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.combinatorics "0.0.8"]]
  :profiles {:dev {
                   :dependencies [[cider/cider-nrepl "0.7.0"]
                                  [midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]] }}
  :repl-options {:nrepl-middleware
                 [cider.nrepl.middleware.classpath/wrap-classpath
                  cider.nrepl.middleware.complete/wrap-complete
                  cider.nrepl.middleware.info/wrap-info
                  cider.nrepl.middleware.inspect/wrap-inspect
                  cider.nrepl.middleware.macroexpand/wrap-macroexpand
                  cider.nrepl.middleware.stacktrace/wrap-stacktrace
                  cider.nrepl.middleware.trace/wrap-trace]} )
