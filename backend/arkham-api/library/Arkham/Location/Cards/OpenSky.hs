module Arkham.Location.Cards.OpenSky (openSky) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ObsidianCanyons.Helpers (canEnterOpenSky)

newtype OpenSky = OpenSky LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openSky :: LocationCard OpenSky
openSky = location OpenSky Cards.openSky 0 (Static 0)

{- | "Open sky counts as a location for the purposes of card effects, location
adjacency, and for determining distance between locations", so it deliberately
stays visible to ordinary location matchers (unlike Before the Black Throne's
empty space, which 'getLocationsMatching' filters out by card code). What it
does not allow is being entered or investigated.
-}
instance HasAbilities OpenSky where
  getAbilities (OpenSky a) = getAbilities a

instance HasModifiersFor OpenSky where
  getModifiersFor (OpenSky a) = do
    modifySelf a [CannotBeFlipped, CannotBeRevealed]
    -- "Investigators cannot move into open sky unless otherwise indicated by
    -- scenario effects" — those effects mark the investigator instead of having
    -- to reach in here (see 'canEnterOpenSky').
    modifySelect a Anyone [CannotInvestigateLocation a.id]
    modifySelect
      a
      (not_ $ InvestigatorWithModifier canEnterOpenSky)
      [CannotEnter a.id]

instance RunMessage OpenSky where
  runMessage msg (OpenSky attrs) = runQueueT $ OpenSky <$> liftRunMessage msg attrs
