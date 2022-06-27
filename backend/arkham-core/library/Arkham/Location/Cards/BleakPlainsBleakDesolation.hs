module Arkham.Location.Cards.BleakPlainsBleakDesolation
  ( bleakPlainsBleakDesolation
  , BleakPlainsBleakDesolation(..)
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

newtype BleakPlainsBleakDesolation = BleakPlainsBleakDesolation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bleakPlainsBleakDesolation :: LocationCard BleakPlainsBleakDesolation
bleakPlainsBleakDesolation = location
  BleakPlainsBleakDesolation
  Cards.bleakPlainsBleakDesolation
  4
  (PerPlayer 1)
  Square
  [Circle, Triangle, Diamond]

instance HasModifiersFor BleakPlainsBleakDesolation where
  getModifiersFor _ (InvestigatorTarget iid) (BleakPlainsBleakDesolation a) =
    pure $ toModifiers
      a
      [ CannotPlay (CardWithType AssetType <> CardWithTrait Ally)
      | iid `member` locationInvestigators a
      ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BleakPlainsBleakDesolation where
  getAbilities (BleakPlainsBleakDesolation attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage BleakPlainsBleakDesolation where
  runMessage msg (BleakPlainsBleakDesolation attrs) =
    BleakPlainsBleakDesolation <$> runMessage msg attrs
