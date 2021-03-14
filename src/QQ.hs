{-# LANGUAGE TemplateHaskell #-}
module QQ where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Data.String
import Data.Char

-- TOOD:
-- Go from this to a representation with slightly more structure
-- (a la John's papers)
data Doc = DEmpty
         | DText String 
         | Doc :<>: Doc 
         deriving Show

instance Semigroup Doc where
  (<>) = (:<>:)

instance Monoid Doc where
  mempty = DEmpty

instance IsString Doc where
  fromString = DText

data Split = Splice String
           | Quoted String
           deriving Show

splitter :: String -> [Split]
splitter s = quote s ""
  where
    quote "" "" = []
    quote "" str = [Quoted $ reverse str]
    quote ('\\':'$':s) str = quote s ('$':str)
    quote ('$':s) str = Quoted (reverse str) : splice s "" 0
    quote (c:s) str = quote s (c:str)

    splice "" "" _ = []
    splice "" str _ = [Splice $ reverse str]
    splice (c:s) str 0
      | isAlphaNum c = splice s (c:str) 0
      | otherwise    = Splice (reverse str) : quote (c:s) ""
    splice ('(':s) str n = splice s ('(':str) (n+1)
    splice (')':s) str 1 = Splice (reverse (')':str)) : quote s ""
    splice (')':s) str n = splice s (')':str) (n-1)
    splice (c:s) str n = splice s (c:str) n

parseString :: String -> TH.Q TH.Exp
parseString s = go (splitter s)
  where
    go []                = [| DEmpty |]
    go ((Quoted s):spls) = [| DText s <> $(go spls) |]
    go ((Splice s):spls) = case parseExp s of
                              Left failure -> fail failure 
                              Right exp    -> [| $(return exp) <> $(go spls) |]

doc :: QuasiQuoter
doc = QuasiQuoter { quoteExp  = parseString
                  , quotePat  = error "This is not a pattern quoter"
                  , quoteType = error "This is not a type quoter"
                  , quoteDec  = error "This is not a declaration quoter"
                  }
