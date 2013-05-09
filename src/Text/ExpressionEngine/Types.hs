module Text.ExpressionEngine.Types
(
  StateS(..)
, post2NFA
)
where

data StateS a = Char a (Maybe (StateS a)) | SplitS (Maybe (StateS a)) (Maybe (StateS a)) | MatchS

post2NFA chs = post2NFA' chs []
    where
        post2NFA' (c:cs) stack = case c of
            '.' -> post2NFA' cs $ doDot stack
            '|' -> post2NFA' cs $ doOr stack
            '?' -> post2NFA' cs $ doQ stack
            '*' -> post2NFA' cs $ doStar stack
            '+' -> post2NFA' cs $ doPlus stack
            _ -> post2NFA' cs (Char c Nothing : stack)
        post2NFA' [] [nfa] = patch nfa MatchS
        post2NFA' _ _ = error "malformed"
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
        patch (Char c Nothing) s = Char c (Just s)
        patch (Char c (Just s)) s' = Char c $ Just $ patch s s'
        patch (SplitS a b) s = SplitS (patch' a s) (patch' b s)
        patch MatchS _ = error "malformed postfix"
        patch' Nothing s = Just s
        patch' (Just s) s' = Just $ patch s s'