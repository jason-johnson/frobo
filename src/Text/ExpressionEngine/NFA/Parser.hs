module Text.ExpressionEngine.NFA.Parser
(
  parseExpression
, parseExpression'
)
where

import           Control.Applicative
import           Text.Parsec         hiding (many, optional, (<|>))
import qualified Data.Set as S(fromList)
import Text.ExpressionEngine.Types
import qualified Text.ExpressionEngine.Types as T

type ParserState = Int
type ExpParser = Parsec String ParserState
type ExpParserS a = ExpParser (T.State a)

newtype Promise a = Promise { resolvePromise :: (T.State a -> T.State a) }

parseExpression :: String -> T.State a
parseExpression e = case runParser p 1 e e of
        Left err -> error $ show err
        Right r -> r
    where
        p = p_rec_many p_char $ p_end 1

step_index :: ExpParser Int
step_index = do
  index <- getState
  updateState succ
  return index

p_rec_many :: ExpParser (T.State a -> T.State a) -> ExpParserS a -> ExpParserS a
p_rec_many p e = many'
    where
        many' = p_some <|> e
        p_some = p <*> many'

p_end :: Int -> ExpParserS a
p_end n = (Final n <$ char '$') <|> (Accept n <$ eof)

parseExpression e = runParser p 1 e e
    where p = p_regex <* eof

p_regex = do
    branches <- sepBy1 p_branch (char '|')
    return branches

p_branch = many1 p_piece

--p_piece = (p_anchor <|> p_atom)
p_piece = p_char

p_char = do
    c <- p_dot <|> p_literal <?> "character"
    i <- step_index
    return $ Step i c
    where
        p_dot = Any <$ char '.'
        p_literal = Literal <$> noneOf specials
        specials = "^.[$()|*+?{\\"

a :: Integer
a = 1
b :: Integer
b = 2