module Arkham.Location.Cards.LabyrinthineHallsCorpseFilledPath (labyrinthineHallsCorpseFilledPath) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype LabyrinthineHallsCorpseFilledPath = LabyrinthineHallsCorpseFilledPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

labyrinthineHallsCorpseFilledPath :: LocationCard LabyrinthineHallsCorpseFilledPath
labyrinthineHallsCorpseFilledPath =
  location LabyrinthineHallsCorpseFilledPath Cards.labyrinthineHallsCorpseFilledPath 2 (PerPlayer 1)

instance HasAbilities LabyrinthineHallsCorpseFilledPath where
  getAbilities (LabyrinthineHallsCorpseFilledPath a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ FastAbility (ResourceCost 4)

instance RunMessage LabyrinthineHallsCorpseFilledPath where
  runMessage msg l@(LabyrinthineHallsCorpseFilledPath attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      atEndOfRound (attrs.ability 1) do
        investigators <- select $ affectsOthersKnown iid $ NotInvestigator (InvestigatorWithId iid)
        for_ (nonEmpty investigators) \others ->
          chooseOrRunOneM iid $ scope "labyrinthineHalls" do
            questionLabeled' "chooseGainResources"
            targets (toList others) \iid' -> gainResources iid' (attrs.ability 1) 4
      pure l
    _ -> LabyrinthineHallsCorpseFilledPath <$> liftRunMessage msg attrs
