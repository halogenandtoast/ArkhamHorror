module Arkham.Location.Cards.FauborgMarigny
  ( FauborgMarigny(..)
  , fauborgMarigny
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( fauborgMarigny )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype FauborgMarigny = FauborgMarigny LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fauborgMarigny :: LocationCard FauborgMarigny
fauborgMarigny = location FauborgMarigny Cards.fauborgMarigny 4 (Static 0)

instance HasModifiersFor FauborgMarigny where
  getModifiersFor (InvestigatorTarget iid) (FauborgMarigny attrs) =
    pure $ toModifiers
      attrs
      [ ReduceCostOf (CardWithType AssetType) 1
      | iid `member` locationInvestigators attrs
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities FauborgMarigny where
  getAbilities (FauborgMarigny a) =
    withBaseAbilities a $ [locationResignAction a]

instance RunMessage FauborgMarigny where
  runMessage msg (FauborgMarigny attrs) =
    FauborgMarigny <$> runMessage msg attrs
