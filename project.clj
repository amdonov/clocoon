(defproject com.github.amdonov.clocoon/clocoon "0.1.0-SNAPSHOT"
            :description "A framework for mapping URLs to SAX pipelines that is similar to Apache Cocoon but written in Clojure"
            :url "http://github.com/amdonov/clocoon"
            :javac-target "1.7"
            :dependencies [[org.clojure/clojure "1.4.0"]
                           [ring/ring-core "1.1.8"]
                           [org.clojure/tools.logging "0.2.6"]
                           [net.sourceforge.nekohtml/nekohtml "1.9.17"]
                           [org.xhtmlrenderer/flying-saucer-pdf "9.0.1"]
                           [com.sun.xml.fastinfoset/FastInfoset "1.2.12"]]
            :aot [clocoon.filter.BaseFilter])
