{-# LANGUAGE QuasiQuotes #-}
-- | An example module.
module Example  where

import Text.Megaparsec
import Text.RawString.QQ
import Text.Megaparsec.String -- input stream is of the type ‘String’
import qualified Text.Megaparsec.Lexer as L
import Control.Monad (void, join)

ex :: String
ex = [r|
begin
field1 string
begin
  field11 int
  field12 string
end subsection; // optional
end;
|]

data Field = Field String String deriving Show
data Block = Fields [Field] | Block [Field] deriving Show

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

field :: Parser Field
field =  dbg "field" $ do
  sc
  Field <$> someTill ((oneOf' (['a'..'z'] ++ ['0'..'9']))) spaceChar
        <*> some ((oneOf' (['a'..'z'] ++ ['0'..'9'])))

endEof = do
  sc *> string "end" *> char ';' *> sc *> eof
  pure ""

endIdent = do
  string "end" *> sc
  ident <- someTill ((oneOf' (['a'..'z'] ++ ['0'..'9']))) (char ';')
  sc *> eof
  pure ident

block = error "TODO implement"
