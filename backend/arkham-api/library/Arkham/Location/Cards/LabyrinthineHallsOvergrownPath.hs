module Arkham.Location.Cards.LabyrinthineHallsOvergrownPath (labyrinthineHallsOvergrownPath) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype LabyrinthineHallsOvergrownPath = LabyrinthineHallsOvergrownPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

labyrinthineHallsOvergrownPath :: LocationCard LabyrinthineHallsOvergrownPath
labyrinthineHallsOvergrownPath =
  location LabyrinthineHallsOvergrownPath Cards.labyrinthineHallsOvergrownPath 2 (PerPlayer 1)

instance HasAbilities LabyrinthineHallsOvergrownPath where
  getAbilities (LabyrinthineHallsOvergrownPath a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (ActionCost 1)

instance RunMessage LabyrinthineHallsOvergrownPath where
  runMessage msg l@(LabyrinthineHallsOvergrownPath attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      atEndOfRound (attrs.ability 1) do
        investigators <- select $ affectsOthersKnown iid $ NotInvestigator (InvestigatorWithId iid)
        for_ (nonEmpty investigators) \others ->
          chooseOrRunOneM iid $ scope "labyrinthineHalls" do
            questionLabeled' "chooseAdditionalActions"
            targets (toList others) \iid' ->
              nextTurnModifier
                iid'
                (attrs.ability 1)
                iid'
                (AdditionalActions "Labyrinthine Halls" (toSource attrs) 2)
      pure l
    _ -> LabyrinthineHallsOvergrownPath <$> liftRunMessage msg attrs
