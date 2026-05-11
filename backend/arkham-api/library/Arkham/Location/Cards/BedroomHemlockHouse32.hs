module Arkham.Location.Cards.BedroomHemlockHouse32 (bedroomHemlockHouse32) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.HemlockHouse.Helpers (getFloorNumber)
import Arkham.Token (Token (..))

newtype BedroomHemlockHouse32 = BedroomHemlockHouse32 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- "X is 1 more than this location's floor number." Default shroud = 0; the
-- HasModifiersFor instance bumps it to floor + 1 dynamically.
bedroomHemlockHouse32 :: LocationCard BedroomHemlockHouse32
bedroomHemlockHouse32 =
  locationWith BedroomHemlockHouse32 Cards.bedroomHemlockHouse32 0 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor BedroomHemlockHouse32 where
  getModifiersFor (BedroomHemlockHouse32 a) = do
    floorN <- getFloorNumber a.id
    modifySelf a [SetShroud (floorN + 1)]

instance HasAbilities BedroomHemlockHouse32 where
  getAbilities (BedroomHemlockHouse32 a) =
    extendRevealed a
      [ -- "[reaction] After you place a seal on this location: Draw 1 card or
        -- gain 2 resources." Seals are tracked as Resource tokens placed by the
        -- act ability "Place 1 resource on it, as a seal".
        mkAbility a 1
          $ freeReaction
              (PlacedToken #after AnySource (TargetIs $ toTarget a.id) Resource)
      ]

instance RunMessage BedroomHemlockHouse32 where
  runMessage msg l@(BedroomHemlockHouse32 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      chooseOneM iid $ do
        labeled "Draw 1 card" $ drawCards iid (attrs.ability 1) 1
        labeled "Gain 2 resources" $ gainResources iid (attrs.ability 1) 2
      pure l
    _ -> BedroomHemlockHouse32 <$> liftRunMessage msg attrs
