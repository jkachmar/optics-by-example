{-# language
 DerivingStrategies,
 NamedFieldPuns,
 RecordWildCards,
 StrictData,
 ViewPatterns,
 TemplateHaskell
 #-}

module Optics.Chapter3 where

import Relude
import Optics
import qualified Data.Text as Text

-------------------------------------------------------------------------------
-- 3.6 - Virtual Fields
data User = User
  { _firstName :: Text
  , _lastName :: Text
  , _username :: Text
  , _email :: Text
  } deriving stock (Eq, Show)


-- > view fullName (User "Alice" "Robertson" "" "") == "Alice Robertson"
-- > set fullName "Bob Allison" (User "Alice" "Roberts" "" "") == "Bob Allison"
fullName :: Lens' User Text
fullName = lens getter setter
  where
    getter User{..} = _firstName <> " " <> _lastName
    setter user@User{..} name = 
      let 
        -- ViewPatterns are very silly...
        (_firstName, Text.stripStart -> _lastName) = Text.breakOn " " name
      in user{ _firstName, _lastName }

-------------------------------------------------------------------------------
-- 3.7 - Data Correction and Maintaining Invariants

data ProducePrices = ProducePrices
  { _lemonPrice :: Float
  , _limePrice :: Float
  } deriving stock (Eq, Show)


-- > set lemonPrice (-1.0) (ProducePrices 1.0 2.0) == ProducePrices 0.0 2.0
lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getter setter
  where
    getter ProducePrices{..} = _lemonPrice
    setter prices _lemonPrice
      | _lemonPrice <= 0.0 = prices{ _lemonPrice = 0.0 }
      | otherwise = prices{ _lemonPrice }


-- > set limePrice (-1.0) (ProducePrices 1.0 2.0) == ProducePrices 1.0 0.0
limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
  where
    getter ProducePrices{..} = _limePrice
    setter prices _limePrice
      | _limePrice <= 0.0 = prices{ _limePrice = 0.0 }
      | otherwise = prices{ _limePrice }
