module Arkham.Location.Cards.Infirmary (
  infirmary,
  Infirmary (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype Infirmary = Infirmary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infirmary :: LocationCard Infirmary
infirmary = location Infirmary Cards.infirmary 3 (PerPlayer 1)

instance HasAbilities Infirmary where
  getAbilities (Infirmary attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (PlayerLimit PerRound 1)
        $ restrictedAbility attrs 1 Here (ActionAbility [] $ ActionCost 1)
      | locationRevealed attrs
      ]

instance RunMessage Infirmary where
  runMessage msg l@(Infirmary attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let source = attrs.ability 1
      healDamage <- canHaveDamageHealed source iid
      healHorror <- canHaveHorrorHealed source iid
      player <- getPlayer iid

      push
        $ chooseOne
          player
          [ Label "Heal 1 damage and take 1 direct horror"
              $ [HealDamage (toTarget attrs) source 1 | healDamage]
              <> [directHorror iid source 1]
          , Label "Heal 1 horror and take 1 direct damage"
              $ [HealHorror (toTarget attrs) source 1 | healHorror]
              <> [directDamage iid source 1]
          ]
      pure l
    _ -> Infirmary <$> runMessage msg attrs
