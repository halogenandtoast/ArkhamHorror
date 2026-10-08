module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Locations.ShrineToHydra (shrineToHydra) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (deepOneInvestigator, scenarioI18n)
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.DevilReef qualified as DevilReefLocations
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ShrineToHydra = ShrineToHydra LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrineToHydra :: LocationCard ShrineToHydra
shrineToHydra = location ShrineToHydra Cards.shrineToHydra 4 (PerPlayer 1)

-- | "Salt Marshes is connected to Shrine to Hydra."
instance HasModifiersFor ShrineToHydra where
  getModifiersFor (ShrineToHydra a) =
    modifySelect
      a
      (locationIs DevilReefLocations.saltMarshes <> RevealedLocation)
      [ConnectedToWhen (locationIs DevilReefLocations.saltMarshes) (be a)]

instance HasAbilities ShrineToHydra where
  getAbilities (ShrineToHydra a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted a 1 (Here <> youExist (not_ deepOneInvestigator)) actionAbility
      ]

instance RunMessage ShrineToHydra where
  runMessage msg l@(ShrineToHydra attrs) = runQueueT $ scenarioI18n "returnToDevilReef" $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      push $ PlaceGrid (GridLocation (Pos 0 6) attrs.id)
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scenarioSpecific "aBargain" iid
      pure l
    _ -> ShrineToHydra <$> liftRunMessage msg attrs
