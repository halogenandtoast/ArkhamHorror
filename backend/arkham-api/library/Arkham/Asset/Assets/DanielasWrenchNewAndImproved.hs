module Arkham.Asset.Assets.DanielasWrenchNewAndImproved (danielasWrenchNewAndImproved) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.History
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype DanielasWrenchNewAndImproved = DanielasWrenchNewAndImproved AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danielasWrenchNewAndImproved :: AssetCard DanielasWrenchNewAndImproved
danielasWrenchNewAndImproved = asset DanielasWrenchNewAndImproved Cards.danielasWrenchNewAndImproved

instance HasAbilities DanielasWrenchNewAndImproved where
  getAbilities (DanielasWrenchNewAndImproved a) =
    [ controlled a 1 (exists $ EnemyAt YourLocation <> oneOf [EnemyCanEngage You, EnemyCanAttack You])
        $ freeTrigger (exhaust a)
    , skillTestAbility $ controlled_ a 2 fightAction_
    ]

instance RunMessage DanielasWrenchNewAndImproved where
  runMessage msg a@(DanielasWrenchNewAndImproved attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt YourLocation <> oneOf [EnemyCanEngage You, EnemyCanAttack You]
      chooseTargetM iid enemies \enemy -> do
        engageEnemy iid enemy
        initiateEnemyAttack enemy (attrs.ability 1) iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 2) iid (SkillModifier #combat 2)
      chooseFightEnemy sid iid (attrs.ability 2)
      pure a
    ChoseEnemy sid iid (isAbilitySource attrs 2 -> True) enemy -> do
      enemies <- getHistoryField RoundHistory iid HistoryEnemiesAttackedBy
      when (enemy `elem` enemies) $ skillTestModifier sid (attrs.ability 2) iid (DamageDealt 1)
      pure a
    _ -> DanielasWrenchNewAndImproved <$> liftRunMessage msg attrs
