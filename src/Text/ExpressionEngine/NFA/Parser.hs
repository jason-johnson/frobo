module Text.ExpressionEngine.NFA.Parser
(
  parseExpression
)
where

import           Control.Applicative
import           Text.Parsec         hiding (many, optional, (<|>))
import qualified Data.Set as S(fromList)
import Text.ExpressionEngine.Types
import qualified Text.ExpressionEngine.Types as T

type ParserState = (Int, Int)
type ExpParser = Parsec String ParserState
type ExpParserS a = ExpParser (T.State a)

newtype Promise a = Promise { resolvePromise :: (T.State a -> T.State a) }

parseExpression :: String -> T.State Char
parseExpression e = case runParser p (1, 1) e e of
        Left err -> error $ show err
        Right r -> r
    where
--        p = p_rec_many p_char $ p_end 1
        p = do
            pat <- p_regex
            end <- p_end 1
            return $ pat end

step_index :: ExpParser Int
step_index = do
  (index, _) <- getState
  updateState $ \(i,gi) -> (succ i, gi)
  return index

group_index :: ExpParser Int
group_index = do
  (_, index) <- getState
  updateState $ \(si,gi) -> (si, succ gi)
  return index

p_many1 :: ExpParser (T.State Char -> T.State Char) -> ExpParser (T.State Char -> T.State Char)
p_many1 p = do
    f <- p
    (p_many1 p >>= return . (f .)) <|> return f

p_splitBy1 :: ExpParser (T.State Char -> T.State Char) -> Parsec String ParserState Char -> ExpParser (T.State Char -> T.State Char)
p_splitBy1 p sep = do
    f <- p
    (sep >> p_splitBy1 p sep >>= return . (\f' e -> Split (f e) (f' e))) <|> return f

p_end :: Int -> ExpParser (T.State Char)
p_end n = (Final n <$ char '$') <|> (Accept n <$ eof)


p_regex :: ExpParser (T.State Char -> T.State Char)
p_regex = do
    p_splitBy1 p_branch (char '|')

p_branch = p_many1 p_piece

--p_piece = (p_anchor <|> p_atom)
p_piece = p_char

p_char :: ExpParser (T.State Char -> T.State Char)
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