module Arkham.Act.Cards.MomentOfDoom (momentOfDoom) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MomentOfDoom = MomentOfDoom ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

momentOfDoom :: ActCard MomentOfDoom
momentOfDoom = act (3, A) MomentOfDoom Cards.momentOfDoom Nothing

instance HasAbilities MomentOfDoom where
  getAbilities = actAbilities \a ->
    [ skillTestAbility
        $ restricted
          (proxied (AssetMatcherSource $ assetIs Assets.relicOfAgesRepossessThePast) a)
          1
          ControlsThis
          actionAbility
    , mkAbility a 2 $ Objective $ forced $ ifEnemyDefeated Enemies.yig
    ]

instance RunMessage MomentOfDoom where
  runMessage msg a@(MomentOfDoom attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (AbilitySource p 1) attrs [#willpower, #intellect] (Fixed 4)
      pure a
    PassedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True) -> do
      withLocationOf iid \lid -> do
        yig <- selectJust $ enemyIs Enemies.yig
        iids <- select $ colocatedWith iid <> InvestigatorWithAnyClues
        chooseOrRunOneM iid $ targets iids \iid' -> do
          flipCluesToDoom iid' 1
          moveTokens (attrs.ability 1) iid' lid #doom 1
          nonAttackEnemyDamage (Just iid) attrs 3 yig
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> MomentOfDoom <$> liftRunMessage msg attrs
