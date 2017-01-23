module Parse where --(parseProgram, parseReplLine) where

import AST

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import Text.Parsec ((<|>))
import Text.Parsec.Text (Parser)
import qualified Data.Char as Char

parseTheStuff :: FilePath -> IO (Either P.ParseError Program)
parseTheStuff file =
  P.parse program file <$> T.readFile file

parseProgram :: FilePath -> T.Text -> Either P.ParseError Program
parseProgram file text = P.parse program file text

parseReplLine :: FilePath -> T.Text
  -> Either P.ParseError (Either Definition Expr)
parseReplLine file text = P.parse replLine file text
  where replLine = Left <$> P.try definition
          <|> (Right <$> expression)


textToVarName :: T.Text -> VarName
textToVarName = T.unpack -- for now


openParen = P.spaces >> P.char '(' >> P.spaces

closeParen = P.spaces >> P.char ')' >> P.spaces

varName = (:) <$> P.satisfy varNameLeading <*> P.many (P.satisfy varNameChar)
  where varNameLeading c = varNameChar c && not (Char.isDigit c)
        varNameChar c = not $ Char.isSpace c || c=='(' || c ==')'


program :: Parser Program
program = P.many1 definition

definition :: Parser Definition
definition = openParen *> (global <|> function) <* closeParen

global =
  P.string "global" *> P.space *> P.spaces
     *> (Global <$> varName <*> expression)

function =
  do P.string "define" *> P.space *> P.spaces
     f <- varName
     openParen
     args <- varName `P.sepBy` P.spaces
     closeParen
     e <- expression
     return (Function f args e)

expression =
  P.spaces *> expression' <* P.spaces

expression' =
  (openParen *> appList <* closeParen)
  <|> (Reference <$> varName)
  <|> (Literal <$> number)

number :: Parser Integer
number = read <$> (minus <|> nat)
  where nat = P.many1 P.digit
        minus = (:) <$> P.char '-' <*> nat

-- the fact that none of the builtins start with the
-- same letter is actually necesary to make this nice
appList =
      appListLeading "if" *> (If <$> expression <*> expression <*> expression)
  <|> appListLeading "while" *> (While <$> expression <*> expression)
  <|> appListLeading "block" *> (Block <$> P.many1 expression)
  <|> appListLeading "print" *> (Print <$> expression)
  <|> appListLeading "set" *> (Set <$> (P.spaces *> varName) <*> expression)
  <|> appListLeading "local" *> (Local <$> (P.spaces *> varName) <*> expression)
  <|> Apply <$> (P.spaces *> varName) <*> P.many1 expression

appListLeading :: String -> Parser Char
appListLeading s = P.string s *> P.space
