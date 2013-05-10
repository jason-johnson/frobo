module Text.ExpressionEngine.NFA.Parser
(
  match
, match'
)
where

import qualified Control.Monad.State as S
import Control.Monad.List (ListT(..))
import Text.ExpressionEngine.Types

post2NFA :: [Char] -> StateS Char
post2NFA chs = post2NFA' 1 chs []
    where
        post2NFA' n (c:cs) stack = case c of
            '.' -> post2NFA' n cs $ doDot stack
            '|' -> post2NFA' n cs $ doOr stack
            '?' -> post2NFA' n cs $ doQ stack
            '*' -> post2NFA' n cs $ doStar stack
            '+' -> post2NFA' n cs $ doPlus stack
            _ -> post2NFA' (n+1) cs (Char n c Nothing : stack)
        post2NFA' _ [] [nfa] = patch nfa MatchS
        post2NFA' _ _ _ = error "malformed"
        doDot (b:a:stack) = patch a b : stack
        doDot _ = error "malformed postfix"
        doOr (b:a:stack) = SplitS (Just a) (Just b) : stack
        doOr _ = error "malformed"
        doQ (a:stack) = SplitS (Just a) Nothing : stack
        doQ _ = error "malformed"
        doStar (a:stack) = let
            s = SplitS (Just $ patch a s) Nothing
            in s : stack
        doStar _ = error "malformed"
        doPlus (a:stack) = let
            s = SplitS (Just s') Nothing
            s' = patch a s
            in s' : stack
        doPlus _ = error "malformed"
        patch (Char n c Nothing) s = Char n c (Just s)
        patch (Char n c (Just s)) s' = Char n c $ Just $ patch s s'
        patch (SplitS a b) s = SplitS (patch' a s) (patch' b s)
        patch MatchS _ = error "malformed postfix"
        patch' Nothing s = Just s
        patch' (Just s) s' = Just $ patch s s'

match str ss = S.runState (runListT $ match' (0 :: Int) str ss) []
    where
        match' sc [] (Final _) = ListT . return $ [(sc, "match successful")]
        match' _ [] _ = failMatch
--        match' sc [] thing = [(sc, show thing)]
        match' _ _ (Final _) = failMatch
        match' sc (c:cs) (Step _ m s) = (if comp c m then toList s else failMatch) >>= match' (sc+1) cs
        match' sc cs st@(Split _ _) = toList st >>= match' sc cs
        match' sc cs (OpenGroup t s) = openGroup t sc >> match' sc cs s
        comp c (Literal a) = c == a
        comp _ Any = True
        toList st = ListT . return $ toList' st
        toList' (Split s1 s2) = toList' s1 ++ toList' s2
        toList' st = [st]
        openGroup t sc = S.modify $ (\gs -> (t,sc):gs)
        failMatch = ListT . return $ []

match' str ss = match'' (0 :: Int) str ss
    where
        match'' sc [] (Final _) = [(sc, "match successful")]
        match'' _ [] _ = []
--        match'' sc [] thing = [(sc, show thing)]
        match'' _ _ (Final _) = []
        match'' sc (c:cs) (Step _ m s) = (if comp c m then toList s else []) >>= match'' (sc+1) cs
        match'' sc cs st@(Split _ _) = toList st >>= match'' sc cs
        match'' sc cs (OpenGroup _ s) = match'' sc cs s
        match'' sc cs (CloseGroup _ s) = match'' sc cs s
        comp c (Literal a) = c == a
        comp _ Any = True
        toList (Split s1 s2) = toList s1 ++ toList s2
        toList st = [st]