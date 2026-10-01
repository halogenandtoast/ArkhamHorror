module Arkham.Homebrew.CircusExMortis.Locations.MountainStream_169 (mountainStream_169) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (hasSealedMoonToken, releaseAMoonToken)
import Arkham.Location.Import.Lifted

newtype MountainStream_169 = MountainStream_169 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mountainStream_169 :: LocationCard MountainStream_169
mountainStream_169 = location MountainStream_169 Cards.mountainStream_169 4 (PerPlayer 1)

instance HasAbilities MountainStream_169 where
  getAbilities (MountainStream_169 a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> youExist hasSealedMoonToken) actionAbility

instance RunMessage MountainStream_169 where
  runMessage msg l@(MountainStream_169 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      releaseAMoonToken iid
      pure l
    _ -> MountainStream_169 <$> liftRunMessage msg attrs
