module Arkham.Asset.Assets.KeeperOfTheKeyCelestialWard (keeperOfTheKeyCelestialWard) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Message qualified as Msg

newtype KeeperOfTheKeyCelestialWard = KeeperOfTheKeyCelestialWard AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keeperOfTheKeyCelestialWard :: AssetCard KeeperOfTheKeyCelestialWard
keeperOfTheKeyCelestialWard = assetWith KeeperOfTheKeyCelestialWard Cards.keeperOfTheKeyCelestialWard $ sanityL ?~ 4

instance HasAbilities KeeperOfTheKeyCelestialWard where
  getAbilities (KeeperOfTheKeyCelestialWard x) =
    [ controlled x 1 (DuringSkillTest SkillTestAtYourLocation)
        $ freeReaction
        $ RevealChaosToken #when Anyone #bless
    , restricted x 2 ControlsThis $ forced $ AssetLeavesPlay #when (be x)
    ]

instance RunMessage KeeperOfTheKeyCelestialWard where
  runMessage msg a@(KeeperOfTheKeyCelestialWard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOneToHandle iid (attrs.ability 1)
        $ locationWithDiscoverableCluesBy iid
        <> oneOf [locationWithInvestigator iid, ConnectedFrom (locationWithInvestigator iid)]
      push $ Msg.DealAssetDamage attrs.id (attrs.ability 1) 0 1
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 1 lid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      placeInBonded iid attrs
      pure a
    _ -> KeeperOfTheKeyCelestialWard <$> liftRunMessage msg attrs
