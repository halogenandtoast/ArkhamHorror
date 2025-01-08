module Arkham.Asset.Assets.PeltShipment (peltShipment, PeltShipment (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Placement

newtype PeltShipment = PeltShipment AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peltShipment :: AssetCard PeltShipment
peltShipment = asset PeltShipment Cards.peltShipment

instance HasModifiersFor PeltShipment where
  getModifiersFor (PeltShipment a) = case a.placement of
    StillInHand iid -> modified_ a iid [HandSize (-3)]
    _ -> pure mempty

instance HasAbilities PeltShipment where
  getAbilities (PeltShipment a) =
    [ restrictedAbility a 1 InYourHand
        $ freeReaction
        $ oneOf [GameEnds #when, InvestigatorResigned #when You]
    ]

instance RunMessage PeltShipment where
  runMessage msg a@(PeltShipment attrs) = runQueueT $ case msg of
    InHand _ (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      pure a
    _ -> PeltShipment <$> liftRunMessage msg attrs
