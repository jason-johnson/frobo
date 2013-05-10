module Text.ExpressionEngine.Types
(
  StateS(..)
, post2NFA
)
where

import Data.Set (Set)

data Match a =
      Any
    | Literal a
    | OneOf (Set a)
    | NoneOf (Set a)
    deriving (Show, Eq)

type Tag = Int  -- needed to print since the structure is cyclic

data State a =
      Step Tag (Match a) (State a)
    | Split (State a) (State a)
    | OpenGroup Tag (State a)
    | CloseGroup Tag (State a)
    | Final Tag

stateAny :: Tag -> State a -> State a
stateAny tag st = Step tag Any st

stateLiteral :: Tag -> a -> State a -> State a
stateLiteral tag a st = Step tag (Literal a) st

instance Show a => Show (State a) where
    show nfa = show' nfa []
        where
            show' st@(Step n m s) seen
                | n `elem` seen = showStepStart st ++ "##"
                | otherwise = showStepStart st ++ "(" ++ show' s (n : seen) ++ ")"
            show' (Split s1 s2) seen = "Split (" ++ show' s1 seen ++ ") (" ++ show' s2 seen ++ ")"
            show' (OpenGroup n s) seen = "OpenGroup " ++ show n ++ " (" ++ show' s seen ++ ")"
            show' (CloseGroup n s) seen = "CloseGroup " ++ show n ++ " (" ++ show' s seen ++ ")"
            show' (Final n) _ = "Final " ++ show n
            showStepStart (Step n m _) = "Step " ++ show n ++ " " ++ show m ++ " "

instance Eq a => Eq (State a) where
    l == r = eq l [] r []
        where
            eq (Step lt lm ls) lseen (Step rt rm rs) rseen
                | lt `elem` lseen && rt `elem` rseen = True
                | lt `elem` lseen || rt `elem` rseen = False
                | otherwise = lm == rm && eq ls (lt:lseen) rs (rt:rseen)
            eq (Split ls1 ls2) lseen (Split rs1 rs2) rseen = eq ls1 lseen rs1 rseen && eq ls2 lseen rs2 rseen
            eq (OpenGroup _ ls) lseen (OpenGroup _ rs) rseen = eq ls lseen rs rseen
            eq (CloseGroup _ ls) lseen (CloseGroup _ rs) rseen = eq ls lseen rs rseen
            eq (Final _) _ (Final _) _ = True
            eq _ _ _ _ = False

data StateS a = Char Tag a (Maybe (StateS a)) | SplitS (Maybe (StateS a)) (Maybe (StateS a)) | MatchS

instance Show a => Show (StateS a) where
    show nfa = show' nfa []
        where
            show' st@(Char n a (Just s)) seen
                | n `elem` seen = showCharStart st ++ "##"
                | otherwise = showCharStart st ++ "(" ++ show' s (n : seen) ++ ")"
            show' (SplitS (Just s1) (Just s2)) seen = "SplitS (" ++ show' s1 seen ++ ") (" ++ show' s2 seen ++ ")"
            show' MatchS _ = "MatchS"
            showCharStart (Char n a _) = "Char " ++ show n ++ " " ++ show a ++ " "

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

match str ss = match' str ss
    where
        match' [] (Final _) = ["match successful"]
        match' [] _ = []
        match' _ (Final _) = []
        match' (c:cs) (Step _ m s) = (if comp c m then toList s else []) >>= match' cs
        match' cs st@(Split _ _) = toList st >>= match' cs
        comp c (Literal a) = c == a
        comp _ Any = True
        toList (Split s1 s2) = toList s1 ++ toList s2
        toList st = [st]