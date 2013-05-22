{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Text.ExpressionEngine.NFATest
import {-@ HTF_TESTS @-} Text.ExpressionEngine.NFA.ParserTest
import {-@ HTF_TESTS @-} Text.ExpressionEngine.NFA.MatcherTest
import {-@ HTF_TESTS @-} Text.ExpressionEngine.TypesTest

main :: IO()
main = htfMain htf_importedTests
