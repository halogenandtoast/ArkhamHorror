module Arkham.Homebrew.AgesUnwound.Locations.Classroom_227 (classroom_227) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Query (inTurnOrder)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Classroom_227 = Classroom_227 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Ritual Supplies/. One of three distinct Classrooms, all with symbol T.
classroom_227 :: LocationCard Classroom_227
classroom_227 = setLabel "classroom1" $ location Classroom_227 Cards.classroom_227 4 (PerPlayer 2)

{- | "Haunted - End your turn. /
Forced - At the start of the mythos phase: Each investigator at this location
draws the top card of the encounter deck, in player order."
-}
instance HasAbilities Classroom_227 where
  getAbilities (Classroom_227 a) =
    extendRevealed
      a
      [ restricted a 1 (exists $ InvestigatorAt (be a)) $ forced $ PhaseBegins #when #mythos
      , campaignI18n $ hauntedI "classroomRitualSupplies.haunted" a 2
      ]

instance RunMessage Classroom_227 where
  runMessage msg l@(Classroom_227 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- inTurnOrder =<< select (investigatorAt attrs.id)
      for_ investigators \iid -> drawEncounterCard iid (attrs.ability 1)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      endYourTurn iid
      pure l
    _ -> Classroom_227 <$> liftRunMessage msg attrs
