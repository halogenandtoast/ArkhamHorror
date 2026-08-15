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
schoolGrounds = locationWith SchoolGrounds Cards.schoolGrounds 1 (PerPlayer 2) connectsToAdjacent

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
