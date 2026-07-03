module Arkham.Location.Cards.TheBurningPit (theBurningPit) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Token

newtype TheBurningPit = TheBurningPit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBurningPit :: LocationCard TheBurningPit
theBurningPit = symbolLabel $ location TheBurningPit Cards.theBurningPit 3 (PerPlayer 1)

instance HasAbilities TheBurningPit where
  getAbilities (TheBurningPit a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (factionAgenda RedFaction))
      $ FastAbility
      $ DrawEncounterCardsCost 1

instance RunMessage TheBurningPit where
  runMessage msg l@(TheBurningPit attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectForMaybeM (factionAgenda RedFaction) \agenda ->
        placeTokens (attrs.ability 1) agenda Ward 1
      pure l
    _ -> TheBurningPit <$> liftRunMessage msg attrs
