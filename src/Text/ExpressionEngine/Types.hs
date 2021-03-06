module Text.ExpressionEngine.Types
(
  State(..)
, Match(..)
, stateAny
, stateLiteral
, stateOneOf
, stateOneOf'
, stateNoneOf
, stateNoneOf'
)
where

import Data.Set (Set, fromList)

data Match a =
      Any
    | Literal a
    | OneOf (Set a)
    | NoneOf (Set a)
    deriving (Show, Eq)

type Tag = Int  -- needed to print/compare since the structure is cyclic

data State a =
      Step Tag (Match a) (State a)
    | Split (State a) (State a)
    | OpenGroup Tag (State a)
    | CloseGroup Tag (State a)
    | Accept Tag
    | Final Tag

instance Show a => Show (State a) where
    show nfa = show' nfa []
        where
            show' st@(Step n _ s) seen
                | n `elem` seen = showStepStart st ++ "..."
                | otherwise = showStepStart st ++ "(" ++ show' s (n : seen) ++ ")"
            show' (Split s1 s2) seen = "Split (" ++ show' s1 seen ++ ") (" ++ show' s2 seen ++ ")"
            show' (OpenGroup n s) seen = "OpenGroup " ++ show n ++ " (" ++ show' s seen ++ ")"
            show' (CloseGroup n s) seen = "CloseGroup " ++ show n ++ " (" ++ show' s seen ++ ")"
            show' (Accept n) _ = "Accept " ++ show n
            show' (Final n) _ = "Final " ++ show n
            showStepStart (Step n m _) = "Step " ++ show n ++ " " ++ show m ++ " "
            showStepStart _ = error "impossible, but shuts up warnings"

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
            eq (Accept _) _ (Accept _) _ = True
            eq (Final _) _ (Final _) _ = True
            eq _ _ _ _ = False

-- smart constructors

stateAny :: Tag -> State a -> State a
stateAny tag st = Step tag Any st

stateLiteral :: Tag -> a -> State a -> State a
stateLiteral tag a st = Step tag (Literal a) st

stateOneOf :: Tag -> Set a -> State a -> State a
stateOneOf tag set st = Step tag (OneOf set) st

stateOneOf' :: Ord a => Tag -> [a] -> State a -> State a
stateOneOf' tag as st = Step tag (OneOf . fromList $ as) st

stateNoneOf :: Tag -> Set a -> State a -> State a
stateNoneOf tag set st = Step tag (NoneOf set) st

stateNoneOf' :: Ord a => Tag -> [a] -> State a -> State a
stateNoneOf' tag as st = Step tag (NoneOf . fromList $ as) st