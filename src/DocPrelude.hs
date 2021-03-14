{-# LANGUAGE TemplateHaskell #-}
module DocPrelude where

import QQ

-- TODO Figure out what a nice interface for tables would be
data Table = Table 

toDoc :: Table -> Doc
toDoc = undefined

-- TODO:
-- Things that would be nice to be able to have as library extensions
-- on top of Docs that can be done with neat embedded DSLs:
-- * Maths
-- * Tables
-- * Document Structure (Headings, Paragraphs, etc. etc.) it is debatable
--   whether or not it would be a good idea to have this be a native abstraction
--   in the Doc type instead (cf. the comment in QQ.hs).
