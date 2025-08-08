module Arkham.Enemy.Cards.BroodOfYogSothothAmorphousTerror (broodOfYogSothothAmorphousTerror) where

import Arkham.Ability
import Arkham.Asset.Cards.TheDunwichLegacy qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BroodOfYogSothothAmorphousTerror = BroodOfYogSothothAmorphousTerror EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

broodOfYogSothothAmorphousTerror :: EnemyCard BroodOfYogSothothAmorphousTerror
broodOfYogSothothAmorphousTerror =
  enemy
    BroodOfYogSothothAmorphousTerror
    Cards.broodOfYogSothothAmorphousTerror
    (6, Static 1, 3)
    (1, 2)

instance HasModifiersFor BroodOfYogSothothAmorphousTerror where
  getModifiersFor (BroodOfYogSothothAmorphousTerror a) = do
    healthModifier <- perPlayer 1
    modifySelf
      a
      [ HealthModifier healthModifier
      , CanOnlyBeAttackedByAbilityOn $ singleton Assets.esotericFormula.cardCode
      ]

instance HasAbilities BroodOfYogSothothAmorphousTerror where
  getAbilities (BroodOfYogSothothAmorphousTerror a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ EnemyEnters #after YourLocation (be a)
      , mkAbility a 2
          $ forced
          $ Enters #after You (locationWithEnemy a)
      ]

instance RunMessage BroodOfYogSothothAmorphousTerror where
  runMessage msg e@(BroodOfYogSothothAmorphousTerror attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      iids <- select $ InvestigatorAt $ locationWithEnemy attrs.id
      leadChooseOneAtATimeM do
        targets iids \iid -> do
          sid <- getRandom
          beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> BroodOfYogSothothAmorphousTerror <$> liftRunMessage msg attrs
