module Arkham.Asset.Assets.JosephineHusserlMiffedMatriarch (josephineHusserlMiffedMatriarch) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.TracesToNowhere.Helpers

newtype JosephineHusserlMiffedMatriarch = JosephineHusserlMiffedMatriarch AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

josephineHusserlMiffedMatriarch :: AssetCard JosephineHusserlMiffedMatriarch
josephineHusserlMiffedMatriarch =
  allyWith JosephineHusserlMiffedMatriarch Cards.josephineHusserlMiffedMatriarch (2, 2) noSlots

instance HasAbilities JosephineHusserlMiffedMatriarch where
  getAbilities (JosephineHusserlMiffedMatriarch a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance RunMessage JosephineHusserlMiffedMatriarch where
  runMessage msg a@(JosephineHusserlMiffedMatriarch attrs) = runQueueT $ scenarioI18n $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectForMaybeM (locationIs Locations.historicalSociety) $ place attrs . AtLocation
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      witnessParley sid iid (attrs.ability 1) #willpower
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      codexEntry "codex.josephineHusserl"
      sealSymbolAndClaim attrs #cultist
      pure a
    _ -> JosephineHusserlMiffedMatriarch <$> liftRunMessage msg attrs
