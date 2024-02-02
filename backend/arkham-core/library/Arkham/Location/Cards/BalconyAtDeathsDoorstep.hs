module Arkham.Location.Cards.BalconyAtDeathsDoorstep (
  balconyAtDeathsDoorstep,
  BalconyAtDeathsDoorstep (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.SkillType

newtype BalconyAtDeathsDoorstep = BalconyAtDeathsDoorstep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

balconyAtDeathsDoorstep :: LocationCard BalconyAtDeathsDoorstep
balconyAtDeathsDoorstep =
  location BalconyAtDeathsDoorstep Cards.balconyAtDeathsDoorstep 1 (Static 0)

instance HasAbilities BalconyAtDeathsDoorstep where
  getAbilities (BalconyAtDeathsDoorstep a) =
    withRevealedAbilities
      a
      [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility a 1 Here
          $ ActionAbility [Action.Parley]
          $ ActionCost 1
          <> SkillIconCost 3 (singleton $ SkillIcon SkillIntellect)
      ]

instance RunMessage BalconyAtDeathsDoorstep where
  runMessage msg l@(BalconyAtDeathsDoorstep attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ GainClues iid (toAbilitySource attrs 1) 2
      pure l
    _ -> BalconyAtDeathsDoorstep <$> runMessage msg attrs
