{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Text.ExpressionEngine.NFATest where
import Text.ExpressionEngine.NFA

import Text.ExpressionEngine.Types


import Test.Framework

accept :: (b, (c, d, [State a])) -> Bool
accept (_,(_,_,[Accept _])) = True
accept _ = False

accepts :: (b, (c, d, [State a])) -> Bool
accepts (_,(_,_,(Accept _:_))) = True
accepts _ = False

final :: (b, (c, d, [State a])) -> Bool
final (_,(_,_,[Final _])) = True
final _ = False

finals :: (b, (c, d, [State a])) -> Bool
finals (_,(_,_,(Final _:_))) = True
finals _ = False

passes :: (b, (c, d, [State a])) -> Bool
passes s = accept s || final s

passess :: (b, (c, d, [State a])) -> Bool
passess s = accepts s || finals s

test_concat = do assertBool (accept $ match "abcdef" expr)
    where expr = parseExpression "^abcdef"

-- anchors

test_noCarrot = do
    assertBool (finals $ match "ab" expr)
    assertBool (finals $ match "aab" expr)
    assertBool (finals $ match "?+.XZr Zab" expr)
    assertBool (not $ passess $ match "abc" expr)
    where expr = parseExpression "ab$"

test_carrot = do
    assertBool (finals $ match "ab" expr)
    assertBool (not $ passess $ match "aab" expr)
    assertBool (not $ passess $ match "?+.XZr Zab" expr)
    assertBool (not $ passess $ match "abc" expr)
    where expr = parseExpression "^ab$"

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