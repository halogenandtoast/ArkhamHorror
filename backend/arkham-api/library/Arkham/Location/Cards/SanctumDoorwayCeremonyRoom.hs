module Arkham.Location.Cards.SanctumDoorwayCeremonyRoom (sanctumDoorwayCeremonyRoom) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SanctumDoorwayCeremonyRoom = SanctumDoorwayCeremonyRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanctumDoorwayCeremonyRoom :: LocationCard SanctumDoorwayCeremonyRoom
sanctumDoorwayCeremonyRoom = location SanctumDoorwayCeremonyRoom Cards.sanctumDoorwayCeremonyRoom 3 (PerPlayer 2)

instance HasAbilities SanctumDoorwayCeremonyRoom where
  getAbilities (SanctumDoorwayCeremonyRoom a) =
    extendRevealed1 a
      $ restricted a 1 (oneOf [AssetExists $ assetAt a.id, InvestigatorExists $ investigatorAt a.id])
      $ forced
      $ RoundEnds #when

instance RunMessage SanctumDoorwayCeremonyRoom where
  runMessage msg l@(SanctumDoorwayCeremonyRoom attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      cancelEffect <- selectAny $ investigatorAt attrs.id <> InvestigatorWithTokenKey #skull
      unless cancelEffect do
        investigators <- select $ investigatorAt attrs.id
        for_ investigators \iid -> directHorror iid (attrs.ability 1) 1
        assets <- select $ assetAt attrs.id <> AssetWithSanity
        for_ assets \aid -> dealAssetDirectHorror aid (attrs.ability 1) 1
      pure l
    _ -> SanctumDoorwayCeremonyRoom <$> liftRunMessage msg attrs
