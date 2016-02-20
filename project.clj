(defproject ru.sunsite/cats "0.9.0"
  :description "Extensions for funcool/cats clojure library"
  :url "https://github.com/serioga/clojure-sunsite-cats"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [funcool/cats "1.2.1"]
                 [org.clojure/test.check "0.9.0" :scope "provided"]]
  :global-vars {*warn-on-reflection* true})
