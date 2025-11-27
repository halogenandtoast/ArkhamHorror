module Arkham.Asset.Assets.PocketTelescope (pocketTelescope) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ForMovement
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype PocketTelescope = PocketTelescope AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pocketTelescope :: AssetCard PocketTelescope
pocketTelescope = asset PocketTelescope Cards.pocketTelescope

instance HasAbilities PocketTelescope where
  getAbilities (PocketTelescope a) =
    [ controlled a 1 (exists UnrevealedLocation) $ FastAbility (exhaust a)
    , controlled
        a
        2
        (exists (RevealedLocation <> ConnectedLocation NotForMovement <> InvestigatableLocation))
        investigateAction_
    ]

instance RunMessage PocketTelescope where
  runMessage msg a@(PocketTelescope attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ UnrevealedLocation <> connectedFrom (locationWithInvestigator iid)
      chooseTargetM iid locations $ lookAtRevealed iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <-
        select
          $ RevealedLocation
          <> connectedFrom (locationWithInvestigator iid)
          <> InvestigatableLocation
      chooseTargetM iid locations $ handleTarget iid (attrs.ability 2)
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (LocationTarget lid) -> do
      abilityModifier (AbilityRef (toSource attrs) 2) (attrs.ability 2) iid (AsIfAt lid)
      sid <- getRandom
      pushM $ mkInvestigateLocation sid iid (attrs.ability 2) lid
      pure a
    _ -> PocketTelescope <$> liftRunMessage msg attrs
