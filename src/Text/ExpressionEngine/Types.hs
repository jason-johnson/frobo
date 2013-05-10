module Text.ExpressionEngine.Types
(
  State(..)
, Match(..)
, stateAny
, stateLiteral
, StateS(..)
)
where

import Data.Set (Set)

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
    | Final Tag

instance Show a => Show (State a) where
    show nfa = show' nfa []
        where
            show' st@(Step n _ s) seen
                | n `elem` seen = showStepStart st ++ "##"
                | otherwise = showStepStart st ++ "(" ++ show' s (n : seen) ++ ")"
            show' (Split s1 s2) seen = "Split (" ++ show' s1 seen ++ ") (" ++ show' s2 seen ++ ")"
            show' (OpenGroup n s) seen = "OpenGroup " ++ show n ++ " (" ++ show' s seen ++ ")"
            show' (CloseGroup n s) seen = "CloseGroup " ++ show n ++ " (" ++ show' s seen ++ ")"
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
            eq (Final _) _ (Final _) _ = True
            eq _ _ _ _ = False

data StateS a = Char Tag a (Maybe (StateS a)) | SplitS (Maybe (StateS a)) (Maybe (StateS a)) | MatchS

instance Show a => Show (StateS a) where
    show nfa = show' nfa []
        where
            show' st@(Char n _ (Just s)) seen
                | n `elem` seen = showCharStart st ++ "##"
                | otherwise = showCharStart st ++ "(" ++ show' s (n : seen) ++ ")"
            show' (SplitS (Just s1) (Just s2)) seen = "SplitS (" ++ show' s1 seen ++ ") (" ++ show' s2 seen ++ ")"
            show' MatchS _ = "MatchS"
            show' _ _ = error "something that should be impossible seems to have happened"
            showCharStart (Char n a _) = "Char " ++ show n ++ " " ++ show a ++ " "
            showCharStart _ = error "impossible, but shuts up warnings"

-- smart constructors

stateAny :: Tag -> State a -> State a
stateAny tag st = Step tag Any st

stateLiteral :: Tag -> a -> State a -> State a
stateLiteral tag a st = Step tag (Literal a) st