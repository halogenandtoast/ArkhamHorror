module Arkham.Homebrew.DarkMatter.Locations.SchoolGrounds (schoolGrounds) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype SchoolGrounds = SchoolGrounds LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | "While investigating this location, it gains +1 shroud for each of your
clues."

TODO(homebrew): applied as a flat shroud bump from the largest clue pile among
investigators here — there is no per-investigator shroud modifier.

TODO(homebrew): "[free] If there are no clues on School Grounds: Put the
set-aside A Shimmer in the Wall location into play directly above School
Grounds." is not implemented; that location has no card definition yet.
-}
schoolGrounds :: LocationCard SchoolGrounds
schoolGrounds = location SchoolGrounds Cards.schoolGrounds 1 (PerPlayer 2)

instance HasModifiersFor SchoolGrounds where
  getModifiersFor (SchoolGrounds a) = do
    here <- select $ investigatorAt a.id
    clues <- traverse (field InvestigatorClues) here
    let bump = foldr max 0 clues
    modifySelfWhen a (bump > 0) [ShroudModifier bump]

instance RunMessage SchoolGrounds where
  runMessage msg (SchoolGrounds attrs) = SchoolGrounds <$> runMessage msg attrs
