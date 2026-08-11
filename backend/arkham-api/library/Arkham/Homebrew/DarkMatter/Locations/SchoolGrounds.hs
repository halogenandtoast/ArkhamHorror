module Arkham.Homebrew.DarkMatter.Locations.SchoolGrounds (schoolGrounds) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Grid (updatePosition)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype SchoolGrounds = SchoolGrounds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

schoolGrounds :: LocationCard SchoolGrounds
schoolGrounds = location SchoolGrounds Cards.schoolGrounds 1 (PerPlayer 2)

{- | "While investigating this location, it gains +1 shroud for each of your
clues.

[free] If there are no clues on School Grounds: Put the set-aside A Shimmer in
the Wall location into play directly above School Grounds."

Ability 1 is the shroud bump. It has to be a skill-test-scoped modifier rather
than a standing 'HasModifiersFor' one: "your clues" is the /investigating/
investigator's clue pile, and a location modifier has no investigator to read.
Mirrors 'Arkham.Homebrew.DarkMatter.Locations.AbandonedLander' (same campaign)
and 'Arkham.Location.Cards.ExperimentalTherapiesWard'.

Ability 2 puts "A Shimmer in the Wall" into play. That is not a separate card:
it is the unrevealed name of Entrance Hall (see
'Arkham.Homebrew.DarkMatter.CardDefs.Locations.entranceHall'), which the
scenario sets aside during setup, so it enters play unrevealed and displays as
A Shimmer in the Wall. The set-aside criterion also makes this naturally
once-only. Act 1 then arranges the Undefined Rooms around it.

Per 'rules/glossary/ability.md', an investigator may only activate an ability on
a location they are at, hence 'Here'.
-}
instance HasAbilities SchoolGrounds where
  getAbilities (SchoolGrounds a) =
    extendRevealed
      a
      [ mkAbility a 1
          $ silent
          $ InitiatedSkillTest #when You #any #any (WhileInvestigating $ be a)
      , restricted
          a
          2
          ( Here
              <> thisExists a LocationWithoutClues
              <> exists (SetAsideCardMatch $ cardIs Cards.entranceHall)
          )
          $ FastAbility Free
      ]

instance RunMessage SchoolGrounds where
  runMessage msg l@(SchoolGrounds attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- field InvestigatorClues iid
      when (clues > 0) do
        withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) attrs (ShroudModifier clues)
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      for_ attrs.position \pos -> do
        card <- getSetAsideCard Cards.entranceHall
        placeLocationInGrid_ (updatePosition pos GridUp) card
      pure l
    _ -> SchoolGrounds <$> liftRunMessage msg attrs
