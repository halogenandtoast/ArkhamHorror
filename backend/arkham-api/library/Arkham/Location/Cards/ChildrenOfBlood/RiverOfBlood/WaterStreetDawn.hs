module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.WaterStreetDawn (waterStreetDawn) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype WaterStreetDawn = WaterStreetDawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

waterStreetDawn :: LocationCard WaterStreetDawn
waterStreetDawn = symbolLabel $ location WaterStreetDawn Cards.waterStreetDawn 2 (PerPlayer 1)

instance HasAbilities WaterStreetDawn where
  getAbilities (WaterStreetDawn a) =
    extendRevealed1 a
      $ playerLimit PerTurn
      $ restricted a 1 (Here <> can.search.deck You) actionAbility

instance RunMessage WaterStreetDawn where
  runMessage msg l@(WaterStreetDawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 9] (basic $ #asset <> #ally) (AddFoundToHand iid 1)
      pure l
    _ -> WaterStreetDawn <$> liftRunMessage msg attrs
