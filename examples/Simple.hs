{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Simple where

import QQ
import Documents
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

enumExample :: Doc
enumExample =[doc|
  I've taken the occasion to write a few pargraphs about
  how I've decided to implement this most crazy of typesetting systems.

  Here is a list of the crazy decisions I've made:
  $(begin enum)
    $eitem I have a crazy syntax
    $(begin enum)
      $eitem And some nesting
    $end
    $eitem With crazy dollar signs
  $end
  |]

todoExample :: Doc
todoExample =[doc|
  You might think that what I've written here makes sense.
  $(todo "I don't")
  |]

sectionsExample :: Doc
sectionsExample = [doc|
  $(begin sections)

  $(section "Introduction")
  In this text we discuss how to get rid of LaTeX once and for all. 

  $(section "Background")
  TeX is a horrible type setting system.

  $(section "Method")
  We replace TeX with something even worse, in Haskell.
  |]
