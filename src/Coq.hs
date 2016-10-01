module Coq
  (module Coq.Protocol,
   module Coq.Xml,
   module Coq.XmlAst,
   module Coq.XmlParser)
where

import Coq.Protocol
import Coq.Xml
import Coq.XmlAst hiding (toNode, toText)
import Coq.XmlParser (node)
