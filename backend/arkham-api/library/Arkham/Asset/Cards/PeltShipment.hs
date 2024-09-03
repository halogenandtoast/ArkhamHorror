module Arkham.Asset.Cards.PeltShipment (peltShipment, PeltShipment (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Matcher

newtype PeltShipment = PeltShipment AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peltShipment :: AssetCard PeltShipment
peltShipment = asset PeltShipment Cards.peltShipment

instance HasModifiersFor PeltShipment where
  getModifiersFor (InvestigatorTarget iid) (PeltShipment attrs) = do
    maybeModified attrs do
      liftGuardM $ selectAny $ inHandOf iid <> basic (CardWithId $ toCardId attrs)
      pure [HandSize (-3)]
  getModifiersFor _ _ = pure []

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
