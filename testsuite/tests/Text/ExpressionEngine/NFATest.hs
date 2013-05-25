{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Text.ExpressionEngine.NFATest where
import Text.ExpressionEngine.NFA

import Text.ExpressionEngine.Types
import qualified Data.Map as M (lookup)
import Data.Map (Map)

import Test.Framework

accept' :: (Num a, Ord a) => a -> Maybe (State t1, a, t) -> Bool
accept' 0 (Just (Accept _, n, _)) = n > 1
accept' n (Just (Accept _, n', _)) = n == n'
accept' _ _ = False

accept ::  (Num k, Num a, Ord k, Ord a) => (t, Map k (State t1, a, t2)) -> Bool
accept (_,rm) = accept' 1 (M.lookup 1 rm)
--accept _ = False

accepts :: (Num k, Num a, Ord k, Ord a) => (t, Map k (State t1, a, t2)) -> Bool
accepts (_,rm) = accept' 0 (M.lookup 1 rm)
--accepts _ = False

final' :: (Num a, Ord a) => a -> Maybe (State t1, a, t) -> Bool
final' 0 (Just (Final _, n, _)) = n > 1
final' n (Just (Final _, n', _)) = n == n'
final' _ _ = False

final :: (Num k, Num a, Ord k, Ord a) => (t, Map k (State t1, a, t2)) -> Bool
final (_,rm) = final' 1 (M.lookup 1 rm)
--final _ = False

finals :: (Num k, Num a, Ord k, Ord a) => (t, Map k (State t1, a, t2)) -> Bool
finals (_,rm) = final' 0 (M.lookup 1 rm)
--finals _ = False

passes :: (Num a, Num k, Ord a, Ord k) => (t, Map k (State t1, a, t2)) -> Bool
passes s = accept s || final s

passess :: (Num a, Num k, Ord a, Ord k) => (t, Map k (State t1, a, t2)) -> Bool
passess s = accepts s || finals s

groupResult :: Ord k => k -> (t, (t1, Map k (t3, t4, t2))) -> Maybe (t3, t4)
groupResult t (_, (_,gm)) = fmap (\(s,e,_) -> (s,e)) . M.lookup t $ gm

groupMatch :: (Eq t, Eq t1, Ord k) => k -> (t, t1) -> ([(t2, (t3, Map k (t, t1, t4)))], b) -> Bool
groupMatch t gr = any f . fst
    where
        f st = maybe False (gr ==) $ groupResult t st

noGroupResult :: (Eq t4, Eq t3, Ord k) => k -> ([(t, (t1, Map k (t3, t4, t2)))], b) -> Bool
noGroupResult t = all f . fst
    where
        f st = groupResult t st == Nothing

test_concat :: IO ()
test_concat = do assertBool (accept $ match "abcdef" expr)
    where expr = parseExpression "^abcdef"

-- anchors

test_noCarrot :: IO ()
test_noCarrot = do
    assertBool (final $ match "ab" expr)
    assertBool (final $ match "aab" expr)
    assertBool (final $ match "?+.XZr Zab" expr)
    assertBool (not $ passes $ match "abc" expr)
    where expr = parseExpression "ab$"

test_carrot :: IO ()
test_carrot = do
    assertBool (final $ match "ab" expr)
    assertBool (not $ passes $ match "aab" expr)
    assertBool (not $ passes $ match "?+.XZr Zab" expr)
    assertBool (not $ passes $ match "abc" expr)
    where expr = parseExpression "^ab$"

-- TODO: Make this test pass again.  It will require switching the mater to be:  StateT ListT State, with the inside state (visible to all traversals) receiving only the successes (probably with the group info that goes with it)
test_noDollar :: IO ()
test_noDollar = do
    assertBool (accept $ match "abc" expr)
    assertBool (accept $ match "abcd" expr)
    assertBool (accept $ match "abcdef" expr)
    assertBool (not $ passes $ match "abdc" expr)
    where expr = parseExpression "^abc"

test_dollar :: IO ()
test_dollar = do
    assertBool (final $ match "abc" expr)
    assertBool (not $ passes $ match "abcd" expr)
    assertBool (not $ passes $ match "abcdef" expr)
    assertBool (not $ passes $ match "abdc" expr)
    where expr = parseExpression "^abc$"

-- operators

test_or :: IO ()
test_or = do
    assertBool (accept $ match "abd" expr)
    assertBool (accept $ match "acd" expr)
    assertBool (not $ passes $ match "ad" expr)
    where expr = parseExpression "^a(b|c)d"

test_star :: IO ()
test_star = do
    assertBool (accept $ match "d" expr)
    assertBool (accept $ match "ad" expr)
    assertBool (accept $ match "aad" expr)
    assertBool (accept $ match "aaad" expr)
    assertBool (accept $ match "aaaaaaaaaaaaaaaad" expr)
    where expr = parseExpression "^a*d"

test_plus :: IO ()
test_plus = do
    assertBool (not $ passes $ match "d" expr)
    assertBool (accept $ match "ad" expr)
    assertBool (accept $ match "aad" expr)
    assertBool (accept $ match "aaad" expr)
    assertBool (accept $ match "aaaaaaaaaaaaaaaad" expr)
    where expr = parseExpression "^a+d"

test_question :: IO ()
test_question = do
    assertBool (accept $ match "d" expr)
    assertBool (accept $ match "ad" expr)
    assertBool (not $ passes $ match "aad" expr)
    where expr = parseExpression "^a?d"

test_group :: IO ()
test_group = do
    assertBool (accept $ match "abcd" expr)
    assertBool (accept $ match "axyd" expr)
    assertBool (not $ passes $ match "abyd" expr)
    assertBool (not $ passes $ match "axcd" expr)
    assertBool (not $ passes $ match "ad" expr)
    where expr = parseExpression "^a(bc|xy)d"

test_groupStar :: IO ()
test_groupStar = do
    assertBool (accept $ match "abcd" expr)
    assertBool (accept $ match "abcbcd" expr)
    assertBool (accept $ match "abcbcbcd" expr)
    assertBool (accept $ match "axyd" expr)
    assertBool (accept $ match "axyxyd" expr)
    assertBool (accept $ match "axyxyxyd" expr)
    assertBool (accept $ match "ad" expr)
    assertBool (accept $ match "abcxyd" expr)
    assertBool (accept $ match "abcbcxyxyd" expr)
    assertBool (not $ passes $ match "abyd" expr)
    assertBool (not $ passes $ match "abcbyd" expr)
    assertBool (not $ passes $ match "axcd" expr)
    where expr = parseExpression "^a(bc|xy)*d"
    
-- characters

test_dot :: IO ()
test_dot = do
    assertBool (accept $ match "aac" expr)
    assertBool (accept $ match "abc" expr)
    assertBool (accept $ match "a{c" expr)
    assertBool (not $ passes $ match "ac" expr)
    where expr = parseExpression "^a.c"

test_leftCurly :: IO ()
test_leftCurly = do
    assertBool (accept $ match "a{c" expr)
    assertBool (not $ passes $ match "a{1c" expr)
    where expr = parseExpression "^a{c"

test_escaped :: IO ()
test_escaped = do
    assertBool (accept $ match "a$c" expr)
    assertBool (not $ passes $ match "abc" expr)
    where expr = parseExpression "^a\\$c"

test_range :: IO ()
test_range = do
    assertBool (accept $ match "aac" expr)
    assertBool (accept $ match "abc" expr)
    assertBool (accept $ match "a-c" expr)
    assertBool (accept $ match "a]c" expr)
    assertBool (not $ passes $ match "ac" expr)
    assertBool (not $ passes $ match "adc" expr)
    where expr = parseExpression "^a[]ab-]c"

test_negRange :: IO ()
test_negRange = do
    assertBool (not $ passes $ match "aac" expr)
    assertBool (not $ passes $ match "abc" expr)
    assertBool (not $ passes $ match "a-c" expr)
    assertBool (not $ passes $ match "ac" expr)
    assertBool (accept $ match "adc" expr)
    assertBool (accept $ match "a^c" expr)
    where expr = parseExpression "^a[^ab-]c"

-- group capture

test_starInside :: IO ()
test_starInside = do
    assertBool (accept $ match "abd" expr)
    assertBool (accept $ match "abcd" expr)
    assertBool (accept $ match "abccd" expr)
    assertBool (not $ passes $ match "ad" expr)
    assertBool (not $ passes $ match "acd" expr)
    where expr = parseExpression "^a(bc*)d"

test_starOutside :: IO ()
test_starOutside = do
    assertBool (accept $ match "ad" expr)
    assertBool (accept $ match "abcd" expr)
    assertBool (accept $ match "abcbcd" expr)
    assertBool (not $ passes $ match "abd" expr)
    assertBool (not $ passes $ match "acd" expr)
    where expr = parseExpression "^a(bc)*d"

test_simpleCapture :: IO ()
test_simpleCapture = do
    assertBool (groupMatch 1 (1,7) $ match "abcxybcd" expr)
    assertBool (groupMatch 1 (1,5) $ match "abcxyd" expr)
    assertBool (groupMatch 1 (1,3) $ match "abcd" expr)
    assertBool (noGroupResult 1 $ match "ad" expr)
    where expr = parseExpression "^a(bc|xy)*d"

test_lateCapture :: IO ()
test_lateCapture = do
    assertBool (groupMatch 1 (0,2) $ match "aee" expr)
    assertBool (groupMatch 1 (2,4) $ match "aebee" expr)
    assertBool (groupMatch 1 (4,6) $ match "aebebee" expr)
    assertBool (noGroupResult 1 $ match "ae" expr)
    assertBool (noGroupResult 1 $ match "be" expr)
    where expr = parseExpression "(ae|be)e$"

test_longCapture :: IO ()
test_longCapture = do
    assertBool (groupMatch 1 (1,2) $ match "aaa" expr)
    assertBool (groupMatch 1 (1,3) $ match "aaaa" expr)
    assertBool (groupMatch 1 (1,4) $ match "aaaaa" expr)
    assertBool (noGroupResult 1 $ match "aa" expr)
    where expr = parseExpression "^a(a)*a+$"                -- NOTE: There is currently a bug that if the star were inside the grouping we would get a bogus (1,1) group match.
