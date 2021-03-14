{-# LANGUAGE TemplateHaskell, ConstraintKinds, GADTs #-}
module QQ where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Data.List
import Data.String
import Data.Char
import Data.Typeable
import Data.Dynamic

import Utils
import Documents

data Split = Splice String
           | Quoted String
           deriving Show

splitter :: String -> [Split]
splitter s = quote s ""
  where
    quote "" "" = []
    quote "" str
      | any (not . isSpace) str = [Quoted . dropWhile isSpace . reverse $ dropWhile isSpace str]
      | otherwise = []
    quote ('\\':'$':s) str = quote s ('$':str)
    quote ('$':s) str
      | any (not . isSpace) str = (Quoted . dropWhile isSpace . reverse $ dropWhile isSpace str) : splice s "" 0
      | otherwise = splice s "" 0
    quote (c:s) str = quote s (c:str)

    splice "" "" _ = []
    splice "" str _ = [Splice $ reverse str]
    splice ('(':s) str n = splice s ('(':str) (n+1)
    splice (')':s) str 1 = Splice (reverse (')':str)) : quote s ""
    splice (')':s) str n = splice s (')':str) (n-1)
    splice (c:s) str 0
      | isAlphaNum c = splice s (c:str) 0
      | otherwise    = Splice (reverse str) : quote (c:s) ""
    splice (c:s) str n = splice s (c:str) n

parseString :: String -> TH.Q TH.Exp
parseString s = go (splitter s)
  where
    go []                = [| mempty |]
    go ((Quoted s):spls) = [| fromString s <> $(go spls) |]
    go ((Splice s):spls) = case parseExp s of
      Left failure -> fail failure 
      Right exp    -> [| $(return exp) <> $(go spls) |]

doc :: QuasiQuoter
doc = QuasiQuoter { quoteExp  = parseString
                  , quotePat  = error "This is not a pattern quoter"
                  , quoteType = error "This is not a type quoter"
                  , quoteDec  = error "This is not a declaration quoter"
                  }
