module Arkham.Location.Cards.MoldyHalls (
  moldyHalls,
  MoldyHalls (..),
) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype MoldyHalls = MoldyHalls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moldyHalls :: LocationCard MoldyHalls
moldyHalls = location MoldyHalls Cards.moldyHalls 4 (PerPlayer 1)

instance HasAbilities MoldyHalls where
  getAbilities (MoldyHalls attrs) =
    withBaseAbilities
      attrs
      [ withCriteria (InvestigatorExists $ You <> InvestigatorWithAnyResources) $
          haunted "Lose 3 resources" attrs 1
      ]

instance RunMessage MoldyHalls where
  runMessage msg l@(MoldyHalls attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ LoseResources iid (toSource attrs) 3
      pure l
    _ -> MoldyHalls <$> runMessage msg attrs
