module Main where

import Test.HUnit

import BowlingKata.Day1
import BowlingKata.Day2
import BowlingKata.Day3
import BowlingKata.Day4
import BowlingKata.Day5
import BowlingKata.Day6
import BowlingKata.Day7
import BowlingKata.Day8
import BowlingKata.Day9
import BowlingKata.Day10
import BowlingKata.Day11

import CalculatorKata.Day1
import CalculatorKata.Day2
import CalculatorKata.Day3
import CalculatorKata.Day4
import CalculatorKata.Day5
import CalculatorKata.Day6

main = do
    runTestTT BowlingKata.Day1.tests
    runTestTT BowlingKata.Day2.tests
    runTestTT BowlingKata.Day3.tests
    runTestTT BowlingKata.Day4.tests
    runTestTT BowlingKata.Day5.tests
    runTestTT BowlingKata.Day6.tests
    runTestTT BowlingKata.Day7.tests
    runTestTT BowlingKata.Day8.tests
    runTestTT BowlingKata.Day9.tests
    runTestTT BowlingKata.Day10.tests
    runTestTT BowlingKata.Day11.tests

    runTestTT CalculatorKata.Day1.tests
    runTestTT CalculatorKata.Day2.tests
    runTestTT CalculatorKata.Day3.tests
    runTestTT CalculatorKata.Day4.tests
    runTestTT CalculatorKata.Day5.tests
    runTestTT CalculatorKata.Day6.tests
