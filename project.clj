(defproject dodgeball "0.1.0-SNAPSHOT"
            :description "FIXME: write description"
            :url "http://example.com/FIXME"
            :license {:name "Eclipse Public License"
                      :url "http://www.eclipse.org/legal/epl-v10.html"}

            :source-paths ["src/clj" "src/cljs"]

            :dependencies [[org.clojure/clojure "1.7.0-beta1"]
                           [ring-server "0.4.0"]
                           [cljsjs/react "0.13.1-0"]
                           [reagent "0.5.0"]
                           [reagent-forms "0.5.0"]
                           [reagent-utils "0.1.4"]
                           [org.clojure/clojurescript "0.0-3196" :scope "provided"]
                           [ring "1.3.2"]
                           [ring/ring-defaults "0.1.4"]
                           [prone "0.8.1"]
                           [compojure "1.3.3"]
                           [org.clojure/core.cache "0.6.4"]
                           [com.cemerick/friend "0.2.1"]
                           [selmer "0.8.2"]
                           [environ "1.0.0"]
                           [secretary "1.2.3"]
                           [org.clojure/core.async "0.1.346.0-17112a-alpha"]]

            :plugins [[lein-cljsbuild "1.0.4"]
                      [lein-environ "1.0.0"]
                      [lein-ring "0.9.1"]
                      [lein-asset-minifier "0.2.2"]]

            :ring {:handler dodgeball.handler/app
                   :uberwar-name "dodgeball.war"}

            :min-lein-version "2.5.0"

            :uberjar-name "dodgeball.jar"

            :main dodgeball.server

            :clean-targets ^{:protect false} ["resources/public/js"]

            :minify-assets
            {:assets
             {"resources/public/css/site.min.css" "resources/public/css/site.css"}}

            :cljsbuild {:builds {:app {:source-paths ["src/cljs"]
                                       :compiler {:output-to     "resources/public/js/app.js"
                                                  :output-dir    "resources/public/js/out"
                                                  :asset-path   "js/out"
                                                  :optimizations :none
                                                  :pretty-print  true}}}}

            :profiles {:dev {:repl-options {:init-ns dodgeball.repl
                                            :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

                             :dependencies [[ring-mock "0.1.5"]
                                            [ring/ring-devel "1.3.2"]
                                            [leiningen "2.5.1"]
                                            [figwheel "0.2.7"]
                                            [weasel "0.6.0"]
                                            [com.cemerick/piggieback "0.2.0"]
                                            [org.clojure/tools.nrepl "0.2.10"]
                                            [pjstadig/humane-test-output "0.7.0"]]

                             :source-paths ["env/dev/clj"]
                             :plugins [[lein-figwheel "0.2.7"]
                                       [com.cemerick/clojurescript.test "0.3.2"]]

                             :injections [(require 'pjstadig.humane-test-output)
                                          (pjstadig.humane-test-output/activate!)]

                             :figwheel {:http-server-root "public"
                                        :server-port 3449
                                        :css-dirs ["resources/public/css"]
                                        :ring-handler dodgeball.handler/app}

                             :env {:dev? true}

                             :cljsbuild {:builds {:app {:source-paths ["env/dev/cljs"]
                                                        :compiler {:main "dodgeball.dev"
                                                                   :source-map true}}
                                                  :test {:source-paths ["src/cljs"  "test/cljs"]
                                                         :compiler {:output-to "target/test.js"
                                                                    :optimizations :whitespace
                                                                    :pretty-print true
                                                                    :preamble ["react/react.js"]}}}
                                         :test-commands {"unit" ["phantomjs" :runner
                                                                 "test/vendor/es5-shim.js"
                                                                 "test/vendor/es5-sham.js"
                                                                 "test/vendor/console-polyfill.js"
                                                                 "target/test.js"]}}}

                       :uberjar {:hooks [leiningen.cljsbuild minify-assets.plugin/hooks]
                                 :env {:production true}
                                 :aot :all
                                 :omit-source true
                                 :cljsbuild {:jar true
                                             :builds {:app
                                                      {:source-paths ["env/prod/cljs"]
                                                       :compiler
                                                                     {:optimizations :advanced
                                                                      :pretty-print false}}}}}})
