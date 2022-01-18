import EvalExpr
import Test.HUnit
import Data.Either

formatResult :: String -> String -> String
formatResult calc result = "for fun EvalExpr \"" ++ calc ++ "\", result should be " ++ result

-- #########################

testAddBasic :: Test
testAddBasic = TestCase (assertEqual (formatResult "3+2" "5.00") 5.00 (fromRight 0 (evalExpr "3+2")))

testSubBasic :: Test
testSubBasic = TestCase (assertEqual (formatResult "3-2" "1.00") 1.00 (fromRight 0 (evalExpr "3-2")))

testMulBasic :: Test
testMulBasic = TestCase (assertEqual (formatResult "3*2" "6.00") 6.00 (fromRight 0 (evalExpr "3*2")))

testDivBasic :: Test
testDivBasic = TestCase (assertEqual (formatResult "3/2" "1.50") 1.50 (fromRight 0 (evalExpr "3/2")))

testAllBasic :: Test
testAllBasic = TestCase (assertEqual (formatResult "3+2*4/2-1" "6.00") 6.00 (fromRight 0 (evalExpr "3+2*4/2-1")))

testsBasic :: Test
testsBasic = TestList [
    TestLabel "testAddBasic" testAddBasic,
    TestLabel "testSubBasic" testSubBasic,
    TestLabel "testMulBasic" testMulBasic,
    TestLabel "testDivBasic" testDivBasic,
    TestLabel "testAllBasic" testAllBasic
    ]

-- #########################

testAddAdvance :: Test
testAddAdvance = TestCase (assertEqual (formatResult "3+   2+  2+3" "10.00") 10.00 (fromRight 0 (evalExpr "3+   2+  2+3")))

testSubAdvance :: Test
testSubAdvance = TestCase (assertEqual (formatResult "3 -2 - 21" "-20.00") (-20.00) (fromRight 0 (evalExpr "3 -2 - 21"))) --FAILURE

testMulAdvance :: Test
testMulAdvance = TestCase (assertEqual (formatResult "(2 + 4) * 5" "30.00") 30.00 (fromRight 0 (evalExpr "(2 + 4) * 5")))

testDivAdvance :: Test
testDivAdvance = TestCase (assertEqual (formatResult "(4 / 2) * 5" "10.00") 10.00 (fromRight 0 (evalExpr "(4 / 2) * 5")))

testDivAdvance2 :: Test
testDivAdvance2 = TestCase (assertEqual (formatResult "2/2/2" "0.50") 0.50 (fromRight 0 (evalExpr "2/2/2")))

testPowAdvance :: Test
testPowAdvance = TestCase (assertEqual (formatResult "4^3^2" "262144.00") 262144.00 (fromRight 0 (evalExpr "4^3^2"))) --To Do

testAllAdvance :: Test
testAllAdvance = TestCase (assertEqual (formatResult "3+2*4/2-1" "6.00") 6.00 (fromRight 0 (evalExpr "3+2*4/2-1"))) --To Do

testsAdvance :: Test
testsAdvance = TestList [
    TestLabel "testAddAdvance" testAddAdvance,
    TestLabel "testSubAdvance" testSubAdvance,
    TestLabel "testMulAdvance" testMulAdvance,
    TestLabel "testDivAdvance" testDivAdvance,
    TestLabel "testDivAdvance2" testDivAdvance2,
    TestLabel "testPowAdvance" testPowAdvance,
    TestLabel "testAllAdvance" testAllAdvance
    ]

-- #########################

testBlankMinus1 :: Test
testBlankMinus1 = TestCase (assertEqual (formatResult "- \t \t2" "-2.00") (-2.00) (fromRight 0 (evalExpr "- \t \t2")))

testBlankMinus2 :: Test
testBlankMinus2 = TestCase (assertEqual (formatResult " \t ( \t - \t  2 \t ) \t " "-2.00") (-2.00) (fromRight 0 (evalExpr " \t ( \t - \t  2 \t ) \t ")))

testBlankSingleAdd :: Test
testBlankSingleAdd = TestCase (assertEqual (formatResult "  2 + ( -  2  ) " "0.00") 0.00 (fromRight 0 (evalExpr "  2 + (-  2  ) ")))

testBlankSingleSub :: Test
testBlankSingleSub = TestCase (assertEqual (formatResult "  2 - ( -  2  ) " "4.00") 4.00 (fromRight 0 (evalExpr "  2 - (-  2  ) ")))

testBlankSingleMul :: Test
testBlankSingleMul = TestCase (assertEqual (formatResult "  2 * ( -  2  ) " "-4.00") (-4.00) (fromRight 0 (evalExpr "  2 * (-  2  ) ")))

testBlankSingleDiv :: Test
testBlankSingleDiv = TestCase (assertEqual (formatResult "  2 / ( -  2  ) " "-1.00") (-1.00) (fromRight 0 (evalExpr "  2 / (-  2  ) ")))

testBlankSinglePow :: Test
testBlankSinglePow = TestCase (assertEqual (formatResult "  2 ^ ( -  2  ) " "0.25") 0.25 (fromRight 0 (evalExpr "  2 ^ (-  2  ) ")))

testsBlank :: Test
testsBlank = TestList [
    TestLabel "testBlankMinus1" testBlankMinus1,
    TestLabel "testBlankMinus2" testBlankMinus2,
    TestLabel "testBlankSingleAdd" testBlankSingleAdd,
    TestLabel "testBlankSingleSub" testBlankSingleSub,
    TestLabel "testBlankSingleMul" testBlankSingleMul,
    TestLabel "testBlankSingleDiv" testBlankSingleDiv,
    TestLabel "testBlankSinglePow" testBlankSinglePow
    ]

-- To add: "2+" "2*" "2-", number + blank, power + blank

-- #########################

testAllMixed1 :: Test
testAllMixed1 = TestCase (assertEqual (formatResult "2^2^2^2 + 4 - 3 * 4+4" "65532") 65532 (fromRight 0 (evalExpr "2^2^2^2 + 4 - 3 * 4+4")))

testAllMixed2 :: Test
testAllMixed2 = TestCase (assertEqual (formatResult "(3 * 3 + (4 - 3)) * 3^2" "90") 90 (fromRight 0 (evalExpr "(3 * 3 + (4 - 3)) * 3^2")))

testAllMixed3 :: Test
testAllMixed3 = TestCase (assertEqual (formatResult "(3 * 3 + (4 - 3)) * 3^(2-1)" "30") 30 (fromRight 0 (evalExpr "(3 * 3 + (4 - 3)) * 3^(2-1)")))

testAllMixed4 :: Test
testAllMixed4 = TestCase (assertEqual (formatResult "(3 * 3 + (4 - 3)) * 3^((3-1)^2)" "810") 810 (fromRight 0 (evalExpr "(3 * 3 + (4 - 3)) * 3^((3-1)^2)")))

testAllMixed5 :: Test
testAllMixed5 = TestCase (assertEqual (formatResult "(3 * 3 + (4 - 3)) * 3^((3-1)^(-2))" "13.160740129524925") 13.160740129524925 (fromRight 0 (evalExpr "(3 * 3 + (4 - 3)) * 3^((3-1)^(-2))")))


testAllMixed :: Test
testAllMixed = TestList [
    TestLabel "testAllMixed1" testAllMixed1,
    TestLabel "testAllMixed2" testAllMixed2,
    TestLabel "testAllMixed3" testAllMixed3,
    TestLabel "testAllMixed4" testAllMixed4,
    TestLabel "testAllMixed5" testAllMixed5
    ]


-- #########################
-- #########################

main :: IO Counts
main = do _ <- runTestTT testsBasic
          runTestTT testsAdvance
          runTestTT testsBlank
          runTestTT testAllMixed

