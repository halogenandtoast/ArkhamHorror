module Arkham.Homebrew.TheSymphonyOfErichZann.Locations.Auditorium (auditorium) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype Auditorium = Auditorium LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

auditorium :: LocationCard Auditorium
auditorium = location Auditorium Cards.auditorium 3 (PerPlayer 2)

instance HasAbilities Auditorium where
  {- "[action] Draw the top card of the encounter deck: Place clues on this
  location until it has 2 clues per investigator." -}
  getAbilities (Auditorium a) = extend1 a $ restricted a 1 Here actionAbility

instance RunMessage Auditorium where
  runMessage msg l@(Auditorium attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      target <- perPlayer 2
      placeClues (attrs.ability 1) attrs (max 0 (target - attrs.clues))
      pure l
    _ -> Auditorium <$> liftRunMessage msg attrs
