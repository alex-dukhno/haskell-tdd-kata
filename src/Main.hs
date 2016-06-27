module Main where

import Test.HUnit

import BowlingKata.Day1
import BowlingKata.Day2
import BowlingKata.Day3
import BowlingKata.Day4
import BowlingKata.Day5
import BowlingKata.Day6

main = do
    runTestTT BowlingKata.Day1.tests
    runTestTT BowlingKata.Day2.tests
    runTestTT BowlingKata.Day3.tests
    runTestTT BowlingKata.Day4.tests
    runTestTT BowlingKata.Day5.tests
    runTestTT BowlingKata.Day6.tests
