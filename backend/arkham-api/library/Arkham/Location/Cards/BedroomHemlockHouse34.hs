module Arkham.Location.Cards.BedroomHemlockHouse34 (bedroomHemlockHouse34) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.HemlockHouse.Helpers (getFloorNumber)
import Arkham.Token (Token (..))

newtype BedroomHemlockHouse34 = BedroomHemlockHouse34 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroomHemlockHouse34 :: LocationCard BedroomHemlockHouse34
bedroomHemlockHouse34 =
  locationWith BedroomHemlockHouse34 Cards.bedroomHemlockHouse34 0 (PerPlayer 1)
    $ connectsToAdjacent
    . (canBeFlippedL .~ True)

instance HasModifiersFor BedroomHemlockHouse34 where
  getModifiersFor (BedroomHemlockHouse34 a) = do
    floorN <- getFloorNumber a.id
    modifySelf a [SetShroud (floorN + 1)]

instance HasAbilities BedroomHemlockHouse34 where
  getAbilities (BedroomHemlockHouse34 a) =
    extendRevealed a
      [ restricted a 1 Here
          $ freeReaction
              (PlacedToken #after AnySource (TargetIs $ toTarget a.id) Resource)
      ]

instance RunMessage BedroomHemlockHouse34 where
  runMessage msg l@(BedroomHemlockHouse34 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "drawCards" $ drawCards iid (attrs.ability 1) 1
        countVar 2 $ labeled' "gainResources" $ gainResources iid (attrs.ability 1) 2
      pure l
    _ -> BedroomHemlockHouse34 <$> liftRunMessage msg attrs
