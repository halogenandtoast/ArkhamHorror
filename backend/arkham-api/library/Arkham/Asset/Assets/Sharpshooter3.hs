module Arkham.Asset.Assets.Sharpshooter3 (sharpshooter3, Sharpshooter3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Types qualified as Field
import Arkham.Field
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Sharpshooter3 = Sharpshooter3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sharpshooter3 :: AssetCard Sharpshooter3
sharpshooter3 = asset Sharpshooter3 Cards.sharpshooter3

instance HasAbilities Sharpshooter3 where
  getAbilities (Sharpshooter3 a) =
    [ restricted a 1 ControlsThis
        $ triggered
          (ActivateAbility #when You $ AssetAbility #firearm <> AbilityIsAction #fight)
          (exhaust a)
    ]

instance RunMessage Sharpshooter3 where
  runMessage msg a@(Sharpshooter3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      anyFightableWithEvade <- selectAny $ CanFightEnemy (toAbilitySource attrs 1) <> EnemyWithEvade
      chooseOrRunOneM iid do
        (cardI18n $ labeled' "sharpshooter3.thisAttackUsesAgilityInsteadOfCombatAllModifiersToYourCombat")
          do
            thisSkillTestModifiers
              iid
              attrs
              iid
              [UseSkillInsteadOf #combat #agility, SkillModifiersAffectOtherSkill #combat #agility]
        when anyFightableWithEvade do
          (cardI18n $ labeled' "sharpshooter3.useTheAttackedEnemysEvadeValueForThisAttackInsteadOfTheirFig") do
            thisSkillTestModifier
              iid
              attrs
              iid
              (AlternateFightField (SomeField Field.EnemyEvade))
          (cardI18n $ labeled' "sharpshooter3.doBoth") do
            thisSkillTestModifiers
              iid
              attrs
              iid
              [ UseSkillInsteadOf #combat #agility
              , SkillModifiersAffectOtherSkill #combat #agility
              , AlternateFightField (SomeField Field.EnemyEvade)
              ]
      pure a
    _ -> Sharpshooter3 <$> liftRunMessage msg attrs
