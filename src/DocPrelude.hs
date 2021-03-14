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
data ListMarks = ListMark | ItemMark deriving (Show, Eq, Data, Typeable)

item :: Doc
item = mark ItemMark

list :: Environment
list = Env { name    = "list"
           , execute = execute
           , topDown = True
           , envMark = toDyn ListMark
           }
  where
    execute (Doc bs) = mconcat [ "* " <> Doc bs' | (_, bs') <- splitOn (=? ItemMark) bs ]

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
    execute (Doc bs) = mconcat [ "* " <> Doc bs' | (_, bs') <- splitOn (=? ItemMark) bs ]
