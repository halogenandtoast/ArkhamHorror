{- | Imports Office (011) -- trade clues for a dig through a deck.

@fromTopOfDeck@ carries 'ShuffleBackIn', which is the "shuffle the searched
deck" half of the printed text.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Locations.ImportsOffice_011 (importsOffice_011) where

import Arkham.Ability
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Strategy

newtype ImportsOffice_011 = ImportsOffice_011 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

importsOffice_011 :: LocationCard ImportsOffice_011
importsOffice_011 = location ImportsOffice_011 Cards.importsOffice_011 2 (PerPlayer 2)

instance HasAbilities ImportsOffice_011 where
  getAbilities (ImportsOffice_011 a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "importsOffice.search"
      $ groupLimit PerRound
      $ restricted a 1 Here actionAbility

instance RunMessage ImportsOffice_011 where
  runMessage msg l@(ImportsOffice_011 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- field InvestigatorClues iid
      scenarioI18n
        $ scope "importsOffice"
        $ countVar clues
        $ chooseAmount iid "cluesToPlace" "$clues" 0 clues attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) -> do
      placeCluesOnLocation iid (attrs.ability 1) n
      let x = 3 * n
      when (x > 0) do
        investigators <- select $ investigatorAt attrs
        chooseOneM iid $ targets investigators \i ->
          search i (attrs.ability 1) i [fromTopOfDeck x] (basic $ #asset <> #item) (DrawFound i 1)
      pure l
    _ -> ImportsOffice_011 <$> liftRunMessage msg attrs
