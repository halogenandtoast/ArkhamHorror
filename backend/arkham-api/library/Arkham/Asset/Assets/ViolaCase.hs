module Arkham.Asset.Assets.ViolaCase (violaCase) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Capability
import Arkham.Matcher
import Arkham.Slot
import Arkham.Strategy

newtype ViolaCase = ViolaCase AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

violaCase :: AssetCard ViolaCase
violaCase = asset ViolaCase Cards.violaCase

instance HasAbilities ViolaCase where
  getAbilities (ViolaCase a) =
    [ controlled a 1 (youExist can.search.deck)
        $ freeReaction (PlayCard #after You $ basic (CardWithId a.cardId))
    ]

instance RunMessage ViolaCase where
  runMessage msg a@(ViolaCase attrs) = runQueueT $ case msg of
    CardIsEnteringPlay iid card | card.id == attrs.cardId -> do
      pushAll $ replicate 2 (AddSlot iid #hand $ RestrictedSlot (toSource attrs) #firearm [])
      ViolaCase <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid attrs iid [fromTopOfDeck 9] (basic $ #asset <> #firearm) (DrawFound iid 1)
      pure a
    _ -> ViolaCase <$> liftRunMessage msg attrs
