(defproject wf-engine "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojars.pallix/analemma "1.0.0"]
                 [tikkba "0.5.0"]
                 [lacij "0.9.0"]]
  :main ^:skip-aot wf-engine.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})













