module Arkham.Location.Cards.BedroomHemlockHouse35 (bedroomHemlockHouse35) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.HemlockHouse.Helpers (getFloorNumber)
import Arkham.Token (Token (..))

newtype BedroomHemlockHouse35 = BedroomHemlockHouse35 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroomHemlockHouse35 :: LocationCard BedroomHemlockHouse35
bedroomHemlockHouse35 =
  locationWith BedroomHemlockHouse35 Cards.bedroomHemlockHouse35 0 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor BedroomHemlockHouse35 where
  getModifiersFor (BedroomHemlockHouse35 a) = do
    floorN <- getFloorNumber a.id
    modifySelf a [SetShroud (floorN + 1)]

instance HasAbilities BedroomHemlockHouse35 where
  getAbilities (BedroomHemlockHouse35 a) =
    extendRevealed a
      [ mkAbility a 1
          $ freeReaction
              (PlacedToken #after AnySource (TargetIs $ toTarget a.id) Resource)
      ]

instance RunMessage BedroomHemlockHouse35 where
  runMessage msg l@(BedroomHemlockHouse35 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      chooseOneM iid $ do
        labeled "Draw 1 card" $ drawCards iid (attrs.ability 1) 1
        labeled "Gain 2 resources" $ gainResources iid (attrs.ability 1) 2
      pure l
    _ -> BedroomHemlockHouse35 <$> liftRunMessage msg attrs
