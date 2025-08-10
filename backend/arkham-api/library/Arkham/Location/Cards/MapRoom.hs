module Arkham.Location.Cards.MapRoom (mapRoom) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log

newtype MapRoom = MapRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mapRoom :: LocationCard MapRoom
mapRoom = locationWith MapRoom Cards.mapRoom 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities MapRoom where
  getAbilities (MapRoom attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 Here
      $ actionAbilityWithCost (SpendTokenKeyCost 2 #"-3")

instance RunMessage MapRoom where
  runMessage msg l@(MapRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <- select $ UnrevealedLocation <> not_ "Hidden Tunnel"
      chooseNM iid 3 $ targets ls \lid -> do
        reveal lid
        -- Need to delay discovering until after the location has been revealed
        -- otherwise it will be rejected
        forTarget lid msg
      record TheTeamReadTheMap
      pure l
    ForTarget (LocationTarget lid) (UseThisAbility iid (isSource attrs -> True) 1) -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 1 lid
      pure l
    _ -> MapRoom <$> liftRunMessage msg attrs
