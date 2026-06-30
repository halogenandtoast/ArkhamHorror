module Arkham.Location.Cards.AthenaeumOfTheEmptySky (athenaeumOfTheEmptySky) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Token

newtype AthenaeumOfTheEmptySky = AthenaeumOfTheEmptySky LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

athenaeumOfTheEmptySky :: LocationCard AthenaeumOfTheEmptySky
athenaeumOfTheEmptySky =
  symbolLabel $ location AthenaeumOfTheEmptySky Cards.athenaeumOfTheEmptySky 3 (PerPlayer 1)

instance HasAbilities AthenaeumOfTheEmptySky where
  getAbilities (AthenaeumOfTheEmptySky a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (factionAgenda BlueFaction))
      $ FastAbility
      $ DrawEncounterCardsCost 1

instance RunMessage AthenaeumOfTheEmptySky where
  runMessage msg l@(AthenaeumOfTheEmptySky attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectForMaybeM (factionAgenda BlueFaction) \agenda ->
        placeTokens (attrs.ability 1) agenda Ward 1
      pure l
    _ -> AthenaeumOfTheEmptySky <$> liftRunMessage msg attrs
