module Text.ExpressionEngine.Types
(
  StateS(..)
, post2NFA
)
where


type Tag = Int  -- needed to print since the structure is cyclic

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