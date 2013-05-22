module Text.ExpressionEngine.NFA.Matcher
(
  match
, match'
)
where

import qualified Control.Monad.State as S
import Control.Monad.List (ListT(..))
import Text.ExpressionEngine.Types
import Data.Set (member, notMember)
import Data.Map (Map, (!))
import qualified Data.Map as M (empty, insertWith)

type Tag = Int

type Result a = (State a, Int)
type Results a = Map Tag (Result a)

type Start = Int
type End = Int

type GroupResult = (Start, End, [End])
type GroupResults = Map Tag GroupResult

type GroupStart = (Start, [Start])
type GroupStarts = Map Tag GroupStart

-- NOTE: When we finalize this function, change all the aux match'' entries to just be match'.  They would have been match' but that creates a warning because of the benchmark match' we temporarily define
match :: (Ord a, Show a) => [a] -> State a -> ([(Int, [Char])], (GroupStarts, GroupResults, Results a))
match str ss = S.runState (runListT $ match'' (0 :: Int) str ss) (M.empty, M.empty, M.empty)
    where
        match'' sc [] st@(Final _) = ListT $ recordWin st >> return [(sc, "match successful")]
        match'' sc [] st@(Accept _) = ListT $ recordWin st >> return [(sc, "match successful")]
        match'' sc cs st@(Split _ _) = toList st >>= match'' sc cs
        match'' sc cs (OpenGroup t s) = openGroup t sc >> match'' sc cs s
        match'' sc cs (CloseGroup t s) = closeGroup t sc >> match'' sc cs s
        match'' _ [] _ = failMatch
--        match'' sc [] thing = ListT . return $ [(sc, show thing)]
        match'' _ _ (Final _) = failMatch
        match'' _ _ st@(Accept _) = recordWin st >> failMatch
        match'' sc (c:cs) (Step _ m s) = (if comp c m then toList s else failMatch) >>= match'' (sc+1) cs
        comp c (Literal a) = c == a
        comp _ Any = True
        comp c (OneOf s) = c `member` s
        comp c (NoneOf s) = c `notMember` s
        toList st = ListT . return $ toList' st
        toList' (Split s1 s2) = toList' s1 ++ toList' s2
        toList' st = [st]
        openGroup t sc = S.modify $ (\(ogm, gm, rm) -> (openGroup' t sc ogm, gm, rm))
        openGroup' t sc ogm = M.insertWith (\_ (sc', scs) -> (min sc sc', sc:scs)) t (sc, [sc]) ogm
        closeGroup t ec = S.modify $ (\(ogm, gm, rs) -> (ogm, closeGroup' t ec ogm gm, rs))
        closeGroup' t ec ogm gm = M.insertWith (\_ (sc, ec', ecs) -> (sc, max ec ec', ec : ecs)) t (fst (ogm ! t), ec, [ec]) gm
        recordWin st = S.modify $ (\(ogs, gs, rm) -> (ogs, gs, recordWin' st (resultTag st) rm))
        recordWin' st t rm = M.insertWith (\_ (_, c) -> (st, succ c)) t (st, 1) rm
        resultTag (Accept t) = t
        resultTag (Final t) = t
        resultTag _ = error "resultTag called on non-result"
        failMatch = ListT . return $ []

match' :: Ord a => [a] -> State a -> [(Int, [Char])]
match' str ss = match'' (0 :: Int) str ss
    where
        match'' sc [] (Final _) = [(sc, "match successful")]
        match'' sc [] (Accept _) = [(sc, "match successful")]
        match'' _ [] _ = []
--        match'' sc [] thing = [(sc, show thing)]
        match'' _ _ (Final _) = []
        match'' sc _ (Accept _) = [(sc, "match successful")]
        match'' sc (c:cs) (Step _ m s) = (if comp c m then toList s else []) >>= match'' (sc+1) cs
        match'' sc cs st@(Split _ _) = toList st >>= match'' sc cs
        match'' sc cs (OpenGroup _ s) = match'' sc cs s
        match'' sc cs (CloseGroup _ s) = match'' sc cs s
        comp c (Literal a) = c == a
        comp _ Any = True
        comp c (OneOf s) = c `member` s
        comp c (NoneOf s) = c `notMember` s
        toList (Split s1 s2) = toList s1 ++ toList s2
        toList st = [st]
