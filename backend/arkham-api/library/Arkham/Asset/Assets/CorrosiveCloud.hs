module Arkham.Asset.Assets.CorrosiveCloud (corrosiveCloud) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosToken.Types (ChaosTokenFace (..))
import Arkham.Evade
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens)
import Arkham.Matcher

newtype CorrosiveCloud = CorrosiveCloud AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corrosiveCloud :: AssetCard CorrosiveCloud
corrosiveCloud = asset CorrosiveCloud Cards.corrosiveCloud

instance HasAbilities CorrosiveCloud where
  getAbilities (CorrosiveCloud a) =
    [controlled a 1 criteria $ evadeActionWithAlternate_ #agility]
   where
    criteria = if hasUses a then exists (EnemyAt YourLocation) else Never

instance RunMessage CorrosiveCloud where
  runMessage msg a@(CorrosiveCloud attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      spendUses (attrs.ability 1) attrs Charge 1
      sid <- getRandom
      mkChooseEvade sid iid (attrs.ability 1) >>= push
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      selectEach (EnemyAt $ locationWithInvestigator iid) \enemy -> do
        automaticallyEvadeEnemy iid enemy
        nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 enemy
      backlash iid
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> backlash iid >> pure a
    _ -> CorrosiveCloud <$> liftRunMessage msg attrs
   where
    backlash iid = do
      tokens <- getSkillTestRevealedChaosTokens
      when (any ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . (.face)) tokens) do
        selectEach (investigatorAt $ locationWithInvestigator iid) \iid' -> do
          assignDamage iid' (attrs.ability 1) 1
          loseActions iid' (attrs.ability 1) 1
