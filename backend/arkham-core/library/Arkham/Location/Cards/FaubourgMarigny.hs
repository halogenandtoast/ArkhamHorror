module Arkham.Location.Cards.FaubourgMarigny
  ( FaubourgMarigny(..)
  , faubourgMarigny
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( faubourgMarigny )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype FaubourgMarigny = FaubourgMarigny LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

faubourgMarigny :: LocationCard FaubourgMarigny
faubourgMarigny = location FaubourgMarigny Cards.faubourgMarigny 4 (Static 0)

instance HasModifiersFor FaubourgMarigny where
  getModifiersFor (InvestigatorTarget iid) (FaubourgMarigny attrs) =
    pure $ toModifiers
      attrs
      [ ReduceCostOf (CardWithType AssetType) 1
      | iid `member` locationInvestigators attrs
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities FaubourgMarigny where
  getAbilities (FaubourgMarigny a) =
    withBaseAbilities a $ [locationResignAction a]

instance RunMessage FaubourgMarigny where
  runMessage msg (FaubourgMarigny attrs) =
    FaubourgMarigny <$> runMessage msg attrs
