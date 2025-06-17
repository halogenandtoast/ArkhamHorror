module Arkham.Asset.Assets.RaymondLoggins (raymondLoggins) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Token (Token (Truth))

newtype RaymondLoggins = RaymondLoggins AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

raymondLoggins :: AssetCard RaymondLoggins
raymondLoggins = allyWith RaymondLoggins Cards.raymondLoggins (1, 2) noSlots

instance HasAbilities RaymondLoggins where
  getAbilities (RaymondLoggins a) =
    [ reactionAbility
        a
        1
        (assetUseCost a Truth 1)
        (DrawCard #when You (basic NonWeaknessTreachery) EncounterDeck)
        ControlsThis
    ]

instance RunMessage RaymondLoggins where
  runMessage msg a@(RaymondLoggins attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cancelRevelation attrs card
      assignHorror iid attrs 1
      pure a
    _ -> RaymondLoggins <$> liftRunMessage msg attrs
