module Arkham.Asset.Cards.DisciplineAlignmentOfSpirit (
  disciplineAlignmentOfSpirit,
  DisciplineAlignmentOfSpirit (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Modifier

newtype DisciplineAlignmentOfSpirit = DisciplineAlignmentOfSpirit AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disciplineAlignmentOfSpirit :: AssetCard DisciplineAlignmentOfSpirit
disciplineAlignmentOfSpirit = asset DisciplineAlignmentOfSpirit Cards.disciplineAlignmentOfSpirit

instance HasModifiersFor DisciplineAlignmentOfSpirit where
  getModifiersFor (InvestigatorTarget iid) (DisciplineAlignmentOfSpirit a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #willpower 1]
  getModifiersFor _ _ = pure []

instance HasAbilities DisciplineAlignmentOfSpirit where
  getAbilities (DisciplineAlignmentOfSpirit x) =
    [restrictedAbility x 1 ControlsThis actionAbility]

instance RunMessage DisciplineAlignmentOfSpirit where
  runMessage msg a@(DisciplineAlignmentOfSpirit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canHealDamage <- canHaveDamageHealed (attrs.ability 1) iid
      canHealHorror <- canHaveHorrorHealed (attrs.ability 1) iid
      chooseOne iid
        $ [ Label
              "Take 1 direct damage to heal 3 horror"
              $ Msg.directDamage iid (attrs.ability 1) 1
              : [HealHorror (toTarget iid) (attrs.ability 1) 3 | canHealHorror]
          ]
        <> [ Label
              "Take 1 direct horror to heal 3 damage"
              $ Msg.directHorror iid (attrs.ability 1) 1
              : [HealDamage (toTarget iid) (attrs.ability 1) 3 | canHealDamage]
           ]

      flipOverBy iid (attrs.ability 1) attrs

      pure a
    Flip iid _ (isTarget attrs -> True) -> do
      push $ ReplaceInvestigatorAsset iid attrs.id (flipCard $ toCard attrs)
      pure a
    _ -> DisciplineAlignmentOfSpirit <$> liftRunMessage msg attrs
