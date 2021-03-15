{-# LANGUAGE TemplateHaskell
           , DeriveDataTypeable
           , OverloadedStrings
           , FlexibleInstances
           , MultiParamTypeClasses
           , FunctionalDependencies
#-}
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

-- Itemized lists as a library
class List a b | b -> a where
  list :: a -> b

instance List Doc Doc where
  list = execute $ list ()

data ListMarks = ListMark | ItemMark
               deriving (Show, Eq, Data, Typeable)

item :: Doc
item = mark ItemMark

instance List () Environment where
  list () = Env { name    = "list"
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
class EnumEnv a b | b -> a where
  enum :: a -> b

instance EnumEnv Doc Doc where
  enum = execute $ enum ()

data EnumMark = EnumMark | EitemMark Int
              deriving (Show, Eq, Data, Typeable)

eitem :: Doc
eitem = mark (EitemMark 0)

instance EnumEnv () Environment where
  enum () = Env { name    = "enum"
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

class TODO a b | b -> a where
  todo :: a -> b

instance TODO Doc Doc where
  todo = execute $ todo ()

instance TODO () Environment where
  todo () = Env { name = "todo"
                , execute = \ doc -> mconcat [ "TODO"
                                             , newline, "===", newline
                                             , doc
                                             , newline, "===", newline
                                             ]
                , topDown = True
                , envMark = toDyn ()
                }

-- Sections
data SectionMarker = SectionMark { secTitle :: String }
                     deriving (Data, Eq, Show, Typeable)

sections :: () -> Environment
sections _ = Env { name = "sections"
                 , execute = execute
                 , topDown = True
                 , envMark = toDyn () }
             where
                execute (Doc bs) = Doc (go 1 bs)

                go n [] = []
                go n (b@(BMark m) : bs) = case fromDynamic m of
                  Just (SectionMark st) -> BText ("# " ++ show n ++ ") " ++ st) : BNewline : go (n+1) bs
                  Nothing               -> b : go n bs
                go n (b : bs)           = b : go n bs

section :: String -> Doc
section = mark . SectionMark
