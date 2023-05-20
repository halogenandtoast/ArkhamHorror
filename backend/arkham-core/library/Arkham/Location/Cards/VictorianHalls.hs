module Arkham.Location.Cards.VictorianHalls (
  victorianHalls,
  VictorianHalls (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait (Trait (SilverTwilight))

newtype VictorianHalls = VictorianHalls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victorianHalls :: LocationCard VictorianHalls
victorianHalls = location VictorianHalls Cards.victorianHalls 4 (Static 0)

instance HasAbilities VictorianHalls where
  getAbilities (VictorianHalls a) =
    withBaseAbilities
      a
      [ limitedAbility (GroupLimit PerGame 1) $
          restrictedAbility a 1 Here $
            ActionAbility Nothing $
              ActionCost 1
      ]

instance RunMessage VictorianHalls where
  runMessage msg l@(VictorianHalls attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $
        DiscardEncounterUntilFirst (toSource attrs) (Just iid) $
          CardWithTrait SilverTwilight
            <> CardWithType EnemyType
      pure l
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      pushAll [SpawnEnemyAt (EncounterCard ec) (toId attrs), GainClues iid (toAbilitySource attrs 1) 2]
      pure l
    _ -> VictorianHalls <$> runMessage msg attrs
