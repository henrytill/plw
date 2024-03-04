module Language.Base.Parser where

import Language.Base (Info, infoFrom)
import Text.Parsec (getPosition)
import Text.Parsec.String (Parser)

getInfo :: Parser Info
getInfo = infoFrom <$> getPosition
