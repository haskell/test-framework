module Test.Framework.Runners.Console.Statistics (
        TestCount, adjustTestCount, testCountTotal,
        TestStatistics(..), ts_pending_tests, ts_any_failures, initialTestStatistics, showFinalTestStatistics
    ) where

import Test.Framework.Core (TestTypeName)
import Test.Framework.Runners.Console.Table

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid


-- | Records a count of the various kinds of test that have been run
newtype TestCount = TestCount { unTestCount :: Map TestTypeName Int }

testCountTestTypes :: TestCount -> [TestTypeName]
testCountTestTypes = Map.keys . unTestCount

testCountForType :: String -> TestCount -> Int
testCountForType test_type = Map.findWithDefault 0 test_type . unTestCount

adjustTestCount :: String -> Int -> TestCount -> TestCount
adjustTestCount test_type amount = TestCount . Map.insertWith (+) test_type amount . unTestCount


-- | The number of tests of all kinds recorded in the given 'TestCount'
testCountTotal :: TestCount -> Int
testCountTotal = sum . Map.elems . unTestCount

instance Monoid TestCount where
    mempty = TestCount $ Map.empty
    mappend (TestCount tcm1) (TestCount tcm2) = TestCount $ Map.unionWith (+) tcm1 tcm2

minusTestCount :: TestCount -> TestCount -> TestCount
minusTestCount (TestCount tcm1) (TestCount tcm2) = TestCount $ Map.unionWith (-) tcm1 tcm2


-- | Records information about the run of a number of tests, such
-- as how many tests have been run, how many are pending and how
-- many have passed or failed.
data TestStatistics = TestStatistics {
        ts_total_tests :: TestCount,
        ts_run_tests :: TestCount,
        ts_passed_tests :: TestCount,
        ts_failed_tests :: TestCount
    }

ts_pending_tests :: TestStatistics -> TestCount
ts_pending_tests ts = ts_total_tests ts `minusTestCount` ts_run_tests ts

ts_any_failures :: TestStatistics -> Bool
ts_any_failures ts = testCountTotal (ts_failed_tests ts) /= 0

-- | Create some test statistics that simply records the total number of
-- tests to be run, ready to be updated by the actual test runs.
initialTestStatistics :: TestCount -> TestStatistics
initialTestStatistics total_tests = TestStatistics {
        ts_total_tests = total_tests,
        ts_run_tests = mempty,
        ts_passed_tests = mempty,
        ts_failed_tests = mempty
    }

-- | Displays statistics as a string something like this:
--
-- @
--        | Properties | Total
-- -------+------------+------
-- Passed | 9          | 9
-- Failed | 1          | 1
-- -------+------------+------
-- Total  | 10         | 10
-- @
showFinalTestStatistics :: TestStatistics -> String
showFinalTestStatistics ts = renderTable $ [Column label_column, SeperatorColumn] ++ (map Column test_type_columns) ++ [SeperatorColumn, Column total_column]
  where
    test_types = sort $ testCountTestTypes (ts_total_tests ts)
    
    label_column      = [Text "",           Seperator, Text "Passed",                Text "Failed",                Seperator, Text "Total"]
    test_type_columns = [ [Text test_type, Seperator, test_stat ts_passed_tests, test_stat ts_failed_tests, Seperator, test_stat ts_total_tests]
                        | test_type <- test_types
                        , let test_stat = testTypeStat test_type ]
    total_column      = [Text "Total",      Seperator, totalStat ts_passed_tests,    totalStat ts_failed_tests,    Seperator, totalStat ts_total_tests]
    
    totalStat              = stat testCountTotal
    testTypeStat test_type = stat (testCountForType test_type)
    stat kind accessor     = Text (show $ kind $ accessor ts)