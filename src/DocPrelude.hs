{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, OverloadedStrings #-}
module DocPrelude where

import Data.Data
import Data.Typeable
import Data.Dynamic

import Documents
import Utils

-- TODO:
-- Things that would be nice to be able to have as library extensions
-- on top of Docs that can be done with neat embedded DSLs:
-- * Maths
-- * Tables
-- * Document Structure (Headings, Paragraphs, etc. etc.) it is debatable
--   whether or not it would be a good idea to have this be a native abstraction
--   in the Doc type instead (cf. the comment in QQ.hs).
-- * Counters (like for theorems, sections, etc.)

-- Itemized lists as a library
data ListMarks = ListMark | ItemMark
               deriving (Show, Eq, Data, Typeable)

item :: Doc
item = mark ItemMark

list :: Environment
list = Env { name    = "list"
           , execute = execute
           , topDown = True
           , envMark = toDyn ListMark
           }
  where
    go nesting [] = []
    go nesting (b@(BBegin env) : bs) = b : go (nesting+1) bs
    go nesting (BEnd : bs) = BEnd : go (nesting - 1) bs
    go 0 (b : bs)
      | b =? ItemMark = BNewline : BText "* " : go 0 bs
      | otherwise     = b : go 0 bs
    go nesting (b : bs) = b : go nesting bs

    execute (Doc bs) = Doc $ go 0 bs

-- Enumerated lists that support nesting as a library
data EnumMark = EnumMark | EitemMark Int
              deriving (Show, Eq, Data, Typeable)

eitem :: Doc
eitem = mark (EitemMark 0)

enum :: Environment
enum = Env { name    = "enum"
           , execute = execute
           , topDown = True
           , envMark = toDyn EnumMark
           }
  where
    prep n [] = []
    prep n (b@(BBegin env) : bs)
      | fromDynamic (envMark env) == Just EnumMark = b : prep (n+1) bs
      | otherwise = b : prep n bs
    prep n (b@(BMark m) : bs) = case fromDynamic m of
      Just (EitemMark k) -> BMark (toDyn $ EitemMark (n+k)) : prep n bs
      Nothing -> b : prep n bs
    prep n (BEnd : bs) = BEnd : prep (n-1) bs
    prep n (b : bs) = b : prep n bs

    go nesting number [] = []
    go nesting number (b@(BBegin env) : bs) = b : go (nesting+1) number bs
    go 0 number (BEnd : bs) = BEnd : bs
    go nesting number (BEnd : bs) = BEnd : go (nesting - 1) number bs
    go 0 number (b@(BMark m) : bs) = case fromDynamic m of
      Just (EitemMark k) -> BNewline : BText (show k ++ "." ++ show number ++") ") : go 0 (number + 1) bs
      Nothing -> b : go 0 number bs
    go nesting number (b : bs) = b : go nesting number bs

    execute (Doc bs) = Doc $ go 0 0 (prep 0 bs)
