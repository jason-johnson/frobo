module Text.ExpressionEngine.NFA.Parser
(
  parseExpression
)
where

import           Control.Applicative
import           Text.Parsec         hiding (many, optional, (<|>))
import qualified Data.Set as S(fromList)
import Data.Set (Set)
import Text.ExpressionEngine.Types
import qualified Text.ExpressionEngine.Types as T

type ParserState = (Int, Int)
type ExpParser = Parsec String ParserState
type ExpParserS a = ExpParser (T.State a)


parseExpression :: String -> T.State Char
parseExpression e = case runParser p (1, 1) e e of
        Left err -> error $ show err
        Right r -> r
    where
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

p_branch :: ExpParser (T.State Char -> T.State Char)
p_branch = p_many1 p_piece

p_piece :: ExpParser (T.State Char -> T.State Char)
p_piece = p_atom >>= p_post_atom

p_atom :: ExpParser (T.State Char -> T.State Char)
p_atom =  p_group <|> p_bracket <|> p_char <?> "atom"

p_post_atom :: (T.State Char -> T.State Char) -> ExpParser (T.State Char -> T.State Char)
p_post_atom atom =
        (char '*' *> return star)
    <|> (char '+' *> return plus)
    <|> (char '?' *> return question)
    <|> return atom
    where
        star e = let (sp, a) = (Split a e, atom sp) in sp
        plus e = let (sp, a) = (Split a e, atom sp) in a
        question e = let e' = atom e in Split e' e

p_group :: ExpParser (T.State Char -> T.State Char)
p_group = lookAhead (char '(') >> do
  index <- group_index
  re <- between (char '(') (char ')') p_regex
  return $ OpenGroup index . re . CloseGroup index

p_bracket :: ExpParser (T.State Char -> T.State Char)
p_bracket = do
    match <- char '[' *> (p_noneOf <|> p_oneOf)
    i <- step_index
    return $ Step i match
    where
        p_noneOf = NoneOf <$> (char '^' *> p_set)
        p_oneOf = OneOf <$> p_set

p_char :: ExpParser (T.State Char -> T.State Char)
p_char = do
    c <- p_dot <|> p_left_curly <|> p_escaped <|> p_literal <?> "character"
    i <- step_index
    return $ Step i c
    where
        p_dot = Any <$ char '.'
        p_literal = Literal <$> noneOf specials
        p_escaped = Literal <$> (char '\\' *> anyChar)
        p_left_curly = try $ Literal <$> (char '{' <* notFollowedBy digit)
        specials = "^.[$()|*+?{\\"

p_set :: ExpParser (Set Char)
p_set = do
    l <- option [] ((:[]) <$> char ']')
    elems <- (if length l == 1 then many p_elem else many1 p_elem) <* char ']'
    return . S.fromList . concat $ l : elems
    where
        p_elem = p_elem_range <|> p_elem_char <?> "bracket expression"
        p_elem_range = try $ enumFromTo <$> normal <*> end
        p_elem_char = (:[]) <$> (endDash <|> endCarrot <|> normal)
        normal = noneOf specials
        end = char '-' *> noneOf specials
        endDash = char '-' <* lookAhead (char ']')
        endCarrot = char '^' <* lookAhead (option unused (char '-') *> char ']')
        specials = "]-^"
        unused = ' '