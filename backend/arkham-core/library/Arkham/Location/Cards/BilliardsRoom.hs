module Arkham.Location.Cards.BilliardsRoom (
  billiardsRoom,
  BilliardsRoom (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.SkillTest.Base
import Arkham.SkillType

newtype BilliardsRoom = BilliardsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

billiardsRoom :: LocationCard BilliardsRoom
billiardsRoom = location BilliardsRoom Cards.billiardsRoom 3 (Static 0)

instance HasAbilities BilliardsRoom where
  getAbilities (BilliardsRoom a) =
    withBaseAbilities
      a
      [ limitedAbility (PlayerLimit PerRound 1)
          $ restrictedAbility a 1 Here
          $ ActionAbility []
          $ ActionCost 1
      ]

instance RunMessage BilliardsRoom where
  runMessage msg l@(BilliardsRoom attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ BeginSkillTest
        $ initSkillTest
          iid
          (toAbilitySource attrs 1)
          attrs
          SkillAgility
          3
      pure l
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ GainClues iid (toAbilitySource attrs 1) 1
        pure l
    _ -> BilliardsRoom <$> runMessage msg attrs
