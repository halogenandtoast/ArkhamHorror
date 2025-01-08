module Arkham.Location.Cards.BasementHall (basementHall, BasementHall (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BasementHall = BasementHall LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

basementHall :: LocationCard BasementHall
basementHall = location BasementHall Cards.basementHall 4 (PerPlayer 1)

instance HasModifiersFor BasementHall where
  getModifiersFor (BasementHall a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities BasementHall where
  getAbilities (BasementHall attrs) =
    extendRevealed1 attrs $ mkAbility attrs 1 $ forced $ RevealLocation #when Anyone (be attrs)

instance RunMessage BasementHall where
  runMessage msg l@(BasementHall attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      patientConfinements <- shuffle =<< getSetAsideCardsMatching (CardWithTitle "Patient Confinement")
      for_ (withIndex1 patientConfinements) \(idx, confinement) -> do
        locationId <- placeLocation confinement
        setLocationLabel locationId $ "patientConfinement" <> tshow idx
      pure l
    _ -> BasementHall <$> liftRunMessage msg attrs
