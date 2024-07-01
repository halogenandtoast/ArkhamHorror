module Arkham.Asset.Cards.WoundedBystanderOnDeathsDoorstep (
  woundedBystanderOnDeathsDoorstep,
  WoundedBystanderOnDeathsDoorstep (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Token

newtype WoundedBystanderOnDeathsDoorstep = WoundedBystanderOnDeathsDoorstep AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor WoundedBystanderOnDeathsDoorstep where
  getModifiersFor target (WoundedBystanderOnDeathsDoorstep a) =
    modified a [NonDirectDamageMustBeAssignToThisN 1 | isTarget a target]

woundedBystanderOnDeathsDoorstep :: AssetCard WoundedBystanderOnDeathsDoorstep
woundedBystanderOnDeathsDoorstep =
  assetWith WoundedBystanderOnDeathsDoorstep Cards.woundedBystanderOnDeathsDoorstep
    $ (healthL ?~ 5)
    . (tokensL %~ setTokens #damage 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasAbilities WoundedBystanderOnDeathsDoorstep where
  getAbilities (WoundedBystanderOnDeathsDoorstep a) =
    [ restrictedAbility a 1 ControlsThis
        $ forced
        $ InvestigatorWouldTakeDamage #when You AnySource IsNonDirectDamage
    , restrictedAbility a 2 ControlsThis $ forced $ AssetDefeated #when ByAny (be a)
    , restrictedAbility a 3 (exists $ be a <> not_ AssetWithDamage) $ SilentForcedAbility AnyWindow
    ]

instance RunMessage WoundedBystanderOnDeathsDoorstep where
  runMessage msg a@(WoundedBystanderOnDeathsDoorstep attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- handled by modifier
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ SufferTrauma iid 0 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      toDiscardBy iid (attrs.ability 3) attrs
      pure a
    _ -> WoundedBystanderOnDeathsDoorstep <$> liftRunMessage msg attrs
