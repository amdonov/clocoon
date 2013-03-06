(ns clocoon.interop)

(gen-interface
  :name clocoon.IResource
  :methods [[fetch [] clocoon.ISource]
            [fetch [long] clocoon.ISource]])

(gen-interface
  :name clocoon.ISource
  :methods [[getReader [] org.xml.sax.XMLReader]
            [getInputSource [] org.xml.sax.InputSource]
            [getLastModified [] long]])

(gen-interface
  :name clocoon.IFilter
  :methods [[getFilter [] org.xml.sax.XMLFilter]])

(gen-interface
  :name clocoon.ICacheable
  :methods [[isCacheValid [long] boolean]
            [getCacheId [] String]])

(gen-interface
  :name clocoon.ISerializer
  :methods [[handler [java.io.OutputStream] org.xml.sax.ContentHandler]
            [getContentType [] String]])

