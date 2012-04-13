{-# LANGUAGE QuasiQuotes #-}

-- |This modules provides support for using JMacro with Happstack.
--
-- It provides the instance,
--
-- > instance ToMessage JStat 
--
-- Which will serve a 'JStat' value as @text/javascript; charset=UTF-8@.
-- The rendered JavaScript will be wrapped in an anonymous function that is
-- then called, so as to ensure the statements execute in a local scope.
-- An implication of this is that top-level unhygienic variables in JMacro
-- will /not/ be globally available; instead, you should set properties on
-- the global @window@ object.
module Happstack.Server.JMacro where

import qualified Data.ByteString.Char8 as S
import Data.ByteString.Lazy.UTF8       as LB
import Happstack.Server                (ToMessage(..))
import Language.Javascript.JMacro      (JStat(..), renderJs, jmacro, jLam, toStat)
import Text.PrettyPrint                (Style(mode), Mode(OneLineMode), style, renderStyle)

instance ToMessage JStat where
    toContentType _  = S.pack "text/javascript; charset=UTF-8"
    toMessage     js =
        LB.fromString . renderStyle lineStyle . renderJs $ scoped
      where
        lineStyle = style { mode = OneLineMode }
        scoped    = [jmacro| (function { `(js)`; })(); |]
