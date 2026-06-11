module Arkham.Asset.Assets.PocketPortal (pocketPortal) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype PocketPortal = PocketPortal AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pocketPortal :: AssetCard PocketPortal
pocketPortal = asset PocketPortal Cards.pocketPortal

instance HasAbilities PocketPortal where
  getAbilities (PocketPortal a) =
    [ mkAbility a 1 $ forced $ AssetEntersPlay #after (be a)
    , controlled_ a 2 $ forced $ TurnBegins #when You
    ]

instance RunMessage PocketPortal where
  runMessage msg a@(PocketPortal attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ PlaceInvestigator iid (AttachedToAsset attrs.id Nothing)
      push $ ChooseEndTurn iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      locations <- select RevealedLocation
      chooseTargetM iid locations \lid -> push $ PlaceInvestigator iid (AtLocation lid)
      pure a
    _ -> PocketPortal <$> liftRunMessage msg attrs
