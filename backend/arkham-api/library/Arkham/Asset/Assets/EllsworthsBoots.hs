module Arkham.Asset.Assets.EllsworthsBoots (ellsworthsBoots) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype EllsworthsBoots = EllsworthsBoots AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ellsworthsBoots :: AssetCard EllsworthsBoots
ellsworthsBoots = asset EllsworthsBoots Cards.ellsworthsBoots

instance HasAbilities EllsworthsBoots where
  getAbilities (EllsworthsBoots a) =
    [ controlled
        a
        1
        (any_ [AccessibleFrom YourLocation, RevealedLocation <> CanEnterLocation You])
        $ ReactionAbility (DiscoveringLastClue #after You YourLocation) (exhaust a)
    ]

instance RunMessage EllsworthsBoots where
  runMessage msg a@(EllsworthsBoots attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connecting <- select $ AccessibleFrom (locationWithInvestigator iid)
      revealed <-
        select
          $ not_ (AccessibleFrom $ locationWithInvestigator iid)
          <> RevealedLocation
          <> CanEnterLocation (InvestigatorWithId iid)
      chooseOneM iid do
        targets connecting $ moveTo (attrs.ability 1) iid
        targets revealed \loc -> do
          toDiscardBy iid (attrs.ability 1) attrs
          moveTo (attrs.ability 1) iid loc
      pure a
    _ -> EllsworthsBoots <$> liftRunMessage msg attrs
