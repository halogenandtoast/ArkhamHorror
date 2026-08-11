module Arkham.Homebrew.DarkMatter.Locations.DerelictShip (derelictShip) where

import Arkham.Cost (Cost (DrawEncounterCardsCost))
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.Actions (pattern Scan)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype DerelictShip = DerelictShip LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

derelictShip :: LocationCard DerelictShip
derelictShip = location DerelictShip Cards.derelictShip 3 (PerPlayer 1)

{- | "As an additional cost to scan at Derelict Ship, you must draw the top card
of the encounter deck."
-}
instance HasModifiersFor DerelictShip where
  getModifiersFor (DerelictShip a) =
    modifySelect
      a
      (investigatorAt a.id)
      [AdditionalCostToPerformAction (IsAction Scan) (DrawEncounterCardsCost 1)]

instance RunMessage DerelictShip where
  runMessage msg (DerelictShip attrs) = DerelictShip <$> runMessage msg attrs
