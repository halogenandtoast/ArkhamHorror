module Arkham.Asset.Assets.JamesCookieFredericksDubiousChoice (
  jamesCookieFredericksDubiousChoice,
  JamesCookieFredericksDubiousChoice (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Matcher
import Arkham.Modifier

newtype JamesCookieFredericksDubiousChoice = JamesCookieFredericksDubiousChoice AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jamesCookieFredericksDubiousChoice :: AssetCard JamesCookieFredericksDubiousChoice
jamesCookieFredericksDubiousChoice =
  allyWith JamesCookieFredericksDubiousChoice Cards.jamesCookieFredericksDubiousChoice (5, 1) noSlots

instance HasAbilities JamesCookieFredericksDubiousChoice where
  getAbilities (JamesCookieFredericksDubiousChoice a) =
    [restricted a 1 ControlsThis $ fightAction (assetUseCost a Ammo 1 <> exhaust a)]

instance RunMessage JamesCookieFredericksDubiousChoice where
  runMessage msg a@(JamesCookieFredericksDubiousChoice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #combat 6)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      void $ runMaybeT do
        EnemyTarget eid <- MaybeT getSkillTestTarget
        liftGuardM $ eid <!=~> EliteEnemy
        lift $ roundModifier (attrs.ability 1) eid CannotAttack
      pure a
    _ -> JamesCookieFredericksDubiousChoice <$> liftRunMessage msg attrs
