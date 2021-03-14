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
    phase1 n [] = []
    phase1 n (b@(BBegin env) : bs)
      | fromDynamic (envMark env) == Just EnumMark = b : phase1 (n+1) bs
      | otherwise = b : phase1 n bs
    phase1 n (b@(BMark m) : bs) = case fromDynamic m of
      Just (EitemMark k) -> BMark (toDyn $ EitemMark (n+k)) : phase1 n bs
      Nothing -> b : phase1 n bs
    phase1 n (BEnd : bs) = BEnd : phase1 (n-1) bs
    phase1 n (b : bs) = b : phase1 n bs

    phase2 nesting number [] = []
    phase2 nesting number (b@(BBegin env) : bs) = b : phase2 (nesting+1) number bs
    phase2 0 number (BEnd : bs) = BEnd : bs
    phase2 nesting number (BEnd : bs) = BEnd : phase2 (nesting - 1) number bs
    phase2 0 number (b@(BMark m) : bs) = case fromDynamic m of
      Just (EitemMark k) -> BNewline : BText (show k ++ "." ++ show number ++") ") : phase2 0 (number + 1) bs
      Nothing -> b : phase2 0 number bs
    phase2 nesting number (b : bs) = b : phase2 nesting number bs

    execute (Doc bs) = Doc $ phase2 0 0 (phase1 0 bs)
