module Arkham.Location.Cards.BedroomHemlockHouse33 (bedroomHemlockHouse33) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.HemlockHouse.Helpers (getFloorNumber)
import Arkham.Token (Token (..))

newtype BedroomHemlockHouse33 = BedroomHemlockHouse33 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroomHemlockHouse33 :: LocationCard BedroomHemlockHouse33
bedroomHemlockHouse33 =
  locationWith BedroomHemlockHouse33 Cards.bedroomHemlockHouse33 0 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor BedroomHemlockHouse33 where
  getModifiersFor (BedroomHemlockHouse33 a) = do
    floorN <- getFloorNumber a.id
    modifySelf a [SetShroud (floorN + 1)]

instance HasAbilities BedroomHemlockHouse33 where
  getAbilities (BedroomHemlockHouse33 a) =
    extendRevealed a
      [ mkAbility a 1
          $ freeReaction
              (PlacedToken #after AnySource (TargetIs $ toTarget a.id) Resource)
      ]

instance RunMessage BedroomHemlockHouse33 where
  runMessage msg l@(BedroomHemlockHouse33 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      chooseOneM iid $ do
        labeled "Draw 1 card" $ drawCards iid (attrs.ability 1) 1
        labeled "Gain 2 resources" $ gainResources iid (attrs.ability 1) 2
      pure l
    _ -> BedroomHemlockHouse33 <$> liftRunMessage msg attrs
