{-# language
 DerivingStrategies,
 NamedFieldPuns,
 RecordWildCards,
 StrictData,
 ViewPatterns,
 TemplateHaskell
 #-}

module Lens.Chapter4 where

import Relude hiding (Predicate)
import Control.Lens

-------------------------------------------------------------------------------
-- 4.2 - When do we need polymorphic lenses?

newtype Vorpal a = Vorpal { unVorpal :: a }
  deriving stock (Eq, Show)

-- | Correct by construction.
--
-- 'Vorpal' is a @newtype@ wrapper around any polymorphic type, so all we
-- _can_ do here is write a type-changing 'Lens'.
vorpalL :: Lens (Vorpal a0) (Vorpal a1) a0 a1
vorpalL = lens getter setter
  where
    getter = unVorpal
    setter _ y = Vorpal y

-- | Exact same data type as the one presented in Chapter 4.2.
data Preferences a = Preferences
  { _best :: a
  , _worst :: a
  } deriving stock (Eq, Show)

-- | Correct by construction.
--
-- 'preferencesL' gets and sets all values via a tuple type to get around
-- the fact that 'Lens'es can only focus on a single field at a time.
preferencesL :: Lens (Preferences a0) (Preferences a1) (a0, a0) (a1, a1)
preferencesL = lens getter setter
  where
    getter Preferences{..} = (_best, _worst)
    setter preferences (_best, _worst) = preferences{ _best, _worst }


-- | Modified from 'Preferences' to disambiguate each of the fields via
-- separate polymorphic type variables.
data Preferences' a b = Preferences'
  { _best' :: a
  , _worst' :: b
  } deriving stock (Eq, Show)

-- | Correct by construction.
--
-- The '_best\'' field type is modified from @a0@ to @a1@, but the '_worst\''
-- field type is left as-is.
bestL' :: Lens (Preferences' a0 b0) (Preferences' a1 b0) a0 a1
bestL' = lens getter setter
  where
    getter Preferences'{..} = _best'
    setter preferences _best' = preferences{ _best' }

-- | Correct by construction.
--
-- The '_worst\'' field type is modified from @a0@ to @a1@, but the '_best\''
-- field type is left as-is.
worstL' :: Lens (Preferences' a0 b0) (Preferences' a0 b1) b0 b1
worstL' = lens getter setter
  where
    getter Preferences'{..} = _worst'
    setter preferences _worst' = preferences{ _worst' }


-- | Data type given in Chapter 4.2 exercises.
data Result e = Result
  { _lineNumber :: Int
  , _result :: Either e Text
  } deriving stock (Eq, Show)

-- | Correct by construction.
--
-- The only thing that can be done here is to manipulate the values so that
-- they align with the constraints imposed by the type signature.
resultL :: Lens (Result e0) (Result e1) (Either e0 Text) (Either e1 Text)
resultL = lens getter setter
  where
    getter Result{..} = _result
    setter result _result = result { _result }


-- | Data type given in Chapter 4.2 exercises.
newtype Predicate a = Predicate (a -> Bool)

-- | Correct by construction.
--
-- It's not particularly interesting, but all we can do is get and set the
-- predicate function as if it were a concrete value (like the other cases).
predicateL :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicateL = lens getter setter
  where
    getter (Predicate aToBool) = aToBool
    setter _ bToBool = Predicate bToBool
