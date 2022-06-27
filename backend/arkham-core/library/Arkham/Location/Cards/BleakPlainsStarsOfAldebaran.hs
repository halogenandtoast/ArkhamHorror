module Arkham.Location.Cards.BleakPlainsStarsOfAldebaran
  ( bleakPlainsStarsOfAldebaran
  , BleakPlainsStarsOfAldebaran(..)
  ) where

import Arkham.Prelude

import Arkham.Card.CardType
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Target
import Arkham.Trait

newtype BleakPlainsStarsOfAldebaran = BleakPlainsStarsOfAldebaran LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bleakPlainsStarsOfAldebaran :: LocationCard BleakPlainsStarsOfAldebaran
bleakPlainsStarsOfAldebaran = location
  BleakPlainsStarsOfAldebaran
  Cards.bleakPlainsStarsOfAldebaran
  4
  (PerPlayer 1)
  Square
  [Circle, Triangle, Diamond]

instance HasModifiersFor BleakPlainsStarsOfAldebaran where
  getModifiersFor _ (InvestigatorTarget iid) (BleakPlainsStarsOfAldebaran a) =
    pure $ toModifiers
      a
      [ CannotPlay (CardWithType AssetType <> CardWithTrait Ally)
      | iid `member` locationInvestigators a
      ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BleakPlainsStarsOfAldebaran where
  getAbilities (BleakPlainsStarsOfAldebaran attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BleakPlainsStarsOfAldebaran where
  runMessage msg (BleakPlainsStarsOfAldebaran attrs) =
    BleakPlainsStarsOfAldebaran <$> runMessage msg attrs
