module Arkham.Asset.Assets.JamesCookieFredericksDubiousChoiceResolute (
  jamesCookieFredericksDubiousChoiceResolute,
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Modifier

newtype JamesCookieFredericksDubiousChoiceResolute = JamesCookieFredericksDubiousChoiceResolute AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jamesCookieFredericksDubiousChoiceResolute :: AssetCard JamesCookieFredericksDubiousChoiceResolute
jamesCookieFredericksDubiousChoiceResolute =
  allyWith
    JamesCookieFredericksDubiousChoiceResolute
    Cards.jamesCookieFredericksDubiousChoiceResolute
    (6, 1)
    noSlots

instance HasAbilities JamesCookieFredericksDubiousChoiceResolute where
  getAbilities (JamesCookieFredericksDubiousChoiceResolute a) =
    [ restricted a 1 (ControlsThis <> DuringTurn You)
        $ FastAbility' (assetUseCost a Ammo 1 <> exhaust a) [#fight]
    ]

instance RunMessage JamesCookieFredericksDubiousChoiceResolute where
  runMessage msg a@(JamesCookieFredericksDubiousChoiceResolute attrs) = runQueueT $ case msg of
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
    _ -> JamesCookieFredericksDubiousChoiceResolute <$> liftRunMessage msg attrs
