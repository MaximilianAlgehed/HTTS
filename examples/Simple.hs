{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Simple where

import QQ
import DocPrelude

greeting :: Doc -> Doc -> Doc
greeting title name = [doc|Hello $title $name, how are you today?|]

ex0 = greeting "Mr." "Smith"
ex1 = greeting "Dr." "Jones"

listExample :: Doc
listExample =[doc|
  I've taken the occasion to write a few pargraphs about
  how I've decided to implement this most crazy of typesetting systems.

  Here is a list of the crazy decisions I've made:
  $(begin list)
    $item I have a crazy syntax
    $item With crazy dollar signs
  $end
  |]
