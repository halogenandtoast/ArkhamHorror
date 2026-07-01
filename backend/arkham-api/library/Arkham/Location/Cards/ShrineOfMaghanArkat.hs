module Arkham.Location.Cards.ShrineOfMaghanArkat (shrineOfMaghanArkat) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Token

newtype ShrineOfMaghanArkat = ShrineOfMaghanArkat LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrineOfMaghanArkat :: LocationCard ShrineOfMaghanArkat
shrineOfMaghanArkat = symbolLabel $ location ShrineOfMaghanArkat Cards.shrineOfMaghanArkat 3 (PerPlayer 1)

instance HasAbilities ShrineOfMaghanArkat where
  getAbilities (ShrineOfMaghanArkat a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (factionAgenda GreenFaction))
      $ FastAbility
      $ DrawEncounterCardsCost 1

instance RunMessage ShrineOfMaghanArkat where
  runMessage msg l@(ShrineOfMaghanArkat attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectForMaybeM (factionAgenda GreenFaction) \agenda ->
        placeTokens (attrs.ability 1) agenda Ward 1
      pure l
    _ -> ShrineOfMaghanArkat <$> liftRunMessage msg attrs
