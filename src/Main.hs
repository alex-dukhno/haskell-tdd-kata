module Main where

    import Test.Hspec

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
    import CalculatorKata.Day7
    import CalculatorKata.Day8
    import CalculatorKata.Day9
    import CalculatorKata.Day10

    import RomanNumbersKata.Day1
    import RomanNumbersKata.Day2
    import RomanNumbersKata.Day3
    import RomanNumbersKata.Day4

    main = do
        hspec BowlingKata.Day1.tests
        hspec BowlingKata.Day2.tests
        hspec BowlingKata.Day3.tests
        hspec BowlingKata.Day4.tests
        hspec BowlingKata.Day5.tests
        hspec BowlingKata.Day6.tests
        hspec BowlingKata.Day7.tests
        hspec BowlingKata.Day8.tests
        hspec BowlingKata.Day9.tests
        hspec BowlingKata.Day10.tests
        hspec BowlingKata.Day11.tests

        hspec CalculatorKata.Day1.tests
        hspec CalculatorKata.Day2.tests
        hspec CalculatorKata.Day3.tests
        hspec CalculatorKata.Day4.tests
        hspec CalculatorKata.Day5.tests
        hspec CalculatorKata.Day6.tests
        hspec CalculatorKata.Day7.tests
        hspec CalculatorKata.Day8.tests
        hspec CalculatorKata.Day9.tests
        hspec CalculatorKata.Day10.tests

        hspec RomanNumbersKata.Day1.tests
        hspec RomanNumbersKata.Day2.tests
        hspec RomanNumbersKata.Day3.tests
        hspec RomanNumbersKata.Day4.tests
