{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Text.ExpressionEngine.NFATest where
import Text.ExpressionEngine.NFA

import Text.ExpressionEngine.Types
import qualified Data.Map as M (lookup)

import Test.Framework

accept' :: Maybe ((State a), Int) -> Bool
accept' (Just (Accept _, n)) = if n > 1 then error "actually got a state accepted multiple times" else True
accept' _ = False

accept [(_,(_,_,rm))] = accept' (M.lookup 1 rm)
accept _ = False

accepts ((_,(_,_,rm)):_) = accept' (M.lookup 1 rm)
accepts _ = False

final' :: Maybe ((State a), Int) -> Bool
final' (Just (Final _, n)) = if n > 1 then error "actually got a state final multiple times" else True
final' _ = False

final [(_,(_,_,rm))] = final' (M.lookup 1 rm)
final _ = False

finals ((_,(_,_,rm)):_) = final' (M.lookup 1 rm)
finals _ = False

passes s = accept s || final s

passess s = accepts s || finals s

groupResult t (_, (_,gm,_)) = fmap (\(s,e,_) -> (s,e)) . M.lookup t $ gm

groupMatch t gr = any f
    where
        f st = maybe False (gr ==) $ groupResult t st

noGroupResult t = all f
    where
        f st = groupResult t st == Nothing

test_concat = do assertBool (accept $ match "abcdef" expr)
    where expr = parseExpression "^abcdef"

-- anchors

test_noCarrot = do
    assertBool (final $ match "ab" expr)
    assertBool (final $ match "aab" expr)
    assertBool (final $ match "?+.XZr Zab" expr)
    assertBool (not $ passes $ match "abc" expr)
    where expr = parseExpression "ab$"

test_carrot = do
    assertBool (final $ match "ab" expr)
    assertBool (not $ passes $ match "aab" expr)
    assertBool (not $ passes $ match "?+.XZr Zab" expr)
    assertBool (not $ passes $ match "abc" expr)
    where expr = parseExpression "^ab$"

-- TODO: Make this test pass again.  It will require switching the mater to be:  StateT ListT State, with the inside state (visible to all traversals) receiving only the successes (probably with the group info that goes with it)
test_noDollar = do
    assertBool (accept $ match "abc" expr)
    assertBool (accept $ match "abcd" expr)
    assertBool (accept $ match "abcdef" expr)
    assertBool (not $ passes $ match "abdc" expr)
    where expr = parseExpression "^abc"

test_dollar = do
    assertBool (final $ match "abc" expr)
    assertBool (not $ passes $ match "abcd" expr)
    assertBool (not $ passes $ match "abcdef" expr)
    assertBool (not $ passes $ match "abdc" expr)
    where expr = parseExpression "^abc$"

-- operators

test_or = do
    assertBool (accept $ match "abd" expr)
    assertBool (accept $ match "acd" expr)
    assertBool (not $ passes $ match "ad" expr)
    where expr = parseExpression "^a(b|c)d"

test_star = do
    assertBool (accept $ match "d" expr)
    assertBool (accept $ match "ad" expr)
    assertBool (accept $ match "aad" expr)
    assertBool (accept $ match "aaad" expr)
    assertBool (accept $ match "aaaaaaaaaaaaaaaad" expr)
    where expr = parseExpression "^a*d"

test_plus = do
    assertBool (not $ passes $ match "d" expr)
    assertBool (accept $ match "ad" expr)
    assertBool (accept $ match "aad" expr)
    assertBool (accept $ match "aaad" expr)
    assertBool (accept $ match "aaaaaaaaaaaaaaaad" expr)
    where expr = parseExpression "^a+d"

test_question = do
    assertBool (accept $ match "d" expr)
    assertBool (accept $ match "ad" expr)
    assertBool (not $ passes $ match "aad" expr)
    where expr = parseExpression "^a?d"

test_group = do
    assertBool (accept $ match "abcd" expr)
    assertBool (accept $ match "axyd" expr)
    assertBool (not $ passes $ match "abyd" expr)
    assertBool (not $ passes $ match "axcd" expr)
    assertBool (not $ passes $ match "ad" expr)
    where expr = parseExpression "^a(bc|xy)d"

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

test_dot = do
    assertBool (accept $ match "aac" expr)
    assertBool (accept $ match "abc" expr)
    assertBool (accept $ match "a{c" expr)
    assertBool (not $ passes $ match "ac" expr)
    where expr = parseExpression "^a.c"

test_leftCurly = do
    assertBool (accept $ match "a{c" expr)
    assertBool (not $ passes $ match "a{1c" expr)
    where expr = parseExpression "^a{c"

test_escaped = do
    assertBool (accept $ match "a$c" expr)
    assertBool (not $ passes $ match "abc" expr)
    where expr = parseExpression "^a\\$c"

test_range = do
    assertBool (accept $ match "aac" expr)
    assertBool (accept $ match "abc" expr)
    assertBool (accept $ match "a-c" expr)
    assertBool (accept $ match "a]c" expr)
    assertBool (not $ passes $ match "ac" expr)
    assertBool (not $ passes $ match "adc" expr)
    where expr = parseExpression "^a[]ab-]c"

test_negRange = do
    assertBool (not $ passes $ match "aac" expr)
    assertBool (not $ passes $ match "abc" expr)
    assertBool (not $ passes $ match "a-c" expr)
    assertBool (not $ passes $ match "ac" expr)
    assertBool (accept $ match "adc" expr)
    assertBool (accept $ match "a^c" expr)
    where expr = parseExpression "^a[^ab-]c"

-- group capture

test_starInside = do
    assertBool (accept $ match "abd" expr)
    assertBool (accept $ match "abcd" expr)
    assertBool (accept $ match "abccd" expr)
    assertBool (not $ passes $ match "ad" expr)
    assertBool (not $ passes $ match "acd" expr)
    where expr = parseExpression "^a(bc*)d"

test_starOutside = do
    assertBool (accept $ match "ad" expr)
    assertBool (accept $ match "abcd" expr)
    assertBool (accept $ match "abcbcd" expr)
    assertBool (not $ passes $ match "abd" expr)
    assertBool (not $ passes $ match "acd" expr)
    where expr = parseExpression "^a(bc)*d"

test_simpleCapture = do
    assertBool (groupMatch 1 (1,7) $ match "abcxybcd" expr)
    assertBool (groupMatch 1 (1,5) $ match "abcxyd" expr)
    assertBool (groupMatch 1 (1,3) $ match "abcd" expr)
    assertBool (noGroupResult 1 $ match "ad" expr)
    where expr = parseExpression "^a(bc|xy)*d"
