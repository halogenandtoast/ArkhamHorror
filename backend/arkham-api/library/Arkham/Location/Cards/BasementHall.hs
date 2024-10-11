module Arkham.Location.Cards.BasementHall (basementHall, BasementHall (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype BasementHall = BasementHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

basementHall :: LocationCard BasementHall
basementHall = location BasementHall Cards.basementHall 4 (PerPlayer 1)

instance HasModifiersFor BasementHall where
  getModifiersFor (LocationTarget lid) (BasementHall attrs) | lid == toId attrs = do
    toModifiers attrs [Blocked | not attrs.revealed]
  getModifiersFor _ _ = pure []

instance HasAbilities BasementHall where
  getAbilities (BasementHall attrs) =
    extendRevealed1 attrs $ mkAbility attrs 1 $ forced $ RevealLocation #when Anyone (be attrs)

instance RunMessage BasementHall where
  runMessage msg l@(BasementHall attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      patientConfinements <- shuffleM =<< getSetAsideCardsMatching (CardWithTitle "Patient Confinement")
      placements <- for (withIndex1 patientConfinements) $ \(idx, confinement) -> do
        (locationId, locationPlacement) <- placeLocation confinement
        pure [locationPlacement, SetLocationLabel locationId $ "patientConfinement" <> tshow idx]
      pushAll $ concat placements
      pure l
    _ -> BasementHall <$> runMessage msg attrs
