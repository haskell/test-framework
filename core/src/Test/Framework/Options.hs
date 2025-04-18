module Test.Framework.Options where

import Test.Framework.Seed
import Test.Framework.Utilities

import Data.Monoid ( Last(Last, getLast) )
import Data.Semigroup as Sem ( Semigroup((<>)) )


type TestOptions = TestOptions' Maybe
type CompleteTestOptions = TestOptions' K
data TestOptions' f = TestOptions {
        topt_seed :: f Seed,
        -- ^ Seed that should be used to create random numbers for generated tests
        topt_maximum_generated_tests :: f Int,
        -- ^ Maximum number of tests to generate when using something like QuickCheck
        topt_maximum_unsuitable_generated_tests :: f Int,
        -- ^ Maximum number of unsuitable tests to consider before giving up when using something like QuickCheck
        topt_maximum_test_size :: f Int,
        -- ^ Maximum size of generated tests when using something like QuickCheck
        topt_maximum_test_depth :: f Int,
        -- ^ Maximum depth of generated tests when using something like SmallCheck
        topt_timeout :: f (Maybe Int)
        -- ^ The number of microseconds to run tests for before considering them a failure
    }

instance Sem.Semigroup (TestOptions' Maybe) where
    to1 <> to2 = TestOptions {
            topt_seed = getLast (mappendBy (Last . topt_seed) to1 to2),
            topt_maximum_generated_tests = getLast (mappendBy (Last . topt_maximum_generated_tests) to1 to2),
            topt_maximum_unsuitable_generated_tests = getLast (mappendBy (Last . topt_maximum_unsuitable_generated_tests) to1 to2),
            topt_maximum_test_size = getLast (mappendBy (Last . topt_maximum_test_size) to1 to2),
            topt_maximum_test_depth = getLast (mappendBy (Last . topt_maximum_test_depth) to1 to2),
            topt_timeout = getLast (mappendBy (Last . topt_timeout) to1 to2)
        }

instance Monoid (TestOptions' Maybe) where
    mempty = TestOptions {
            topt_seed = Nothing,
            topt_maximum_generated_tests = Nothing,
            topt_maximum_unsuitable_generated_tests = Nothing,
            topt_maximum_test_size = Nothing,
            topt_maximum_test_depth = Nothing,
            topt_timeout = Nothing
        }

    mappend = (Sem.<>)
