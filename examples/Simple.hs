{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Simple where

import QQ

greeting :: Doc -> Doc -> Doc
greeting title name = [doc|Hello $title $name, how are you today?|]

ex0 = greeting "Mr." "Smith"
ex1 = greeting "Dr." "Jones"
