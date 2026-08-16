(defproject DEFN-System "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[cider/cider-nrepl "0.49.0"]]
  ;; Tell Leiningen where to find our custom build of the Z80Processor.
  :repositories [["local" {:url "file:local-libs" 
                           :username "" 
                           :password "" 
                           :checksum :warn
                           :snapshots false}]]
  :dependencies [[org.clojure/clojure "1.11.3"]
                 [seesaw "1.4.5"]
                 [quil "4.3.1563"]
                 [com.esotericsoftware/kryo "5.6.0"]
                  [org.objenesis/objenesis "3.3"]
                 [com.codingrodent.microprocessor/Z80Processor "4.0.0-local"]]
  :main ^:skip-aot z80.core
  ;; :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
