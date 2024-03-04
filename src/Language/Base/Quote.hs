module Language.Base.Quote where

import Language.Haskell.TH qualified as TH
import Text.Parsec (SourcePos, eof, parse, spaces)
import Text.Parsec.Pos (newPos)
import Text.Parsec.String (Parser)

getSourcePos :: TH.Q SourcePos
getSourcePos = f <$> TH.location
  where
    f :: TH.Loc -> SourcePos
    f loc = uncurry (newPos (TH.loc_filename loc)) (TH.loc_start loc)

parseOrError :: (Monad m) => Parser t -> String -> String -> m t
parseOrError p name str = either (error . show) return (parse p name str)

topLevel :: Parser t -> Parser t
topLevel p = spaces *> p <* eof
