{-# LANGUAGE ConstraintKinds, GADTs #-}
module Documents where

import Data.Dynamic
import Data.String
import Data.List
import Data.Char

import Utils

type Mark m = (Typeable m, Eq m)

data Environment = Env { name :: String
                       , execute :: Doc -> Doc
                       , topDown :: Bool
                       , envMark :: Dynamic }

instance Show Environment where
  show env = "Env " ++ show (name env)

data Block where
  BBegin :: Environment -> Block
  BEnd   :: Block
  BMark  :: Dynamic -> Block
  BText  :: String -> Block
  BParbreak :: Block
  deriving Show

(=?) :: Mark m => Block -> m -> Bool
BMark dyn =? m = fromDynamic dyn == Just m
_         =? _ = False

data Doc = Doc { blocks :: [Block] } deriving Show

instance Semigroup Doc where
  Doc bs <> Doc bs' = Doc (bs ++ bs')

instance Monoid Doc where
  mempty = Doc []

mark :: Mark m => m -> Doc
mark m = Doc [BMark (toDyn m)]

begin :: Environment -> Doc
begin env = Doc [BBegin env]

end :: Doc
end = Doc [BEnd]

instance IsString Doc where
  fromString s =
    let ls = lines s
        cleanedUp = map (dropWhile isSpace) ls
        chunks = splitOn (==[]) cleanedUp
    in Doc $ intersperse BParbreak [ BText $ unwords s | (_, s) <- chunks ]
