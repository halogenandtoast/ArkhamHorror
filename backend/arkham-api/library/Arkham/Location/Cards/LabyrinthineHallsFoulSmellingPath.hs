module Arkham.Location.Cards.LabyrinthineHallsFoulSmellingPath (labyrinthineHallsFoulSmellingPath) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype LabyrinthineHallsFoulSmellingPath = LabyrinthineHallsFoulSmellingPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

labyrinthineHallsFoulSmellingPath :: LocationCard LabyrinthineHallsFoulSmellingPath
labyrinthineHallsFoulSmellingPath =
  location LabyrinthineHallsFoulSmellingPath Cards.labyrinthineHallsFoulSmellingPath 2 (PerPlayer 1)

instance HasAbilities LabyrinthineHallsFoulSmellingPath where
  getAbilities (LabyrinthineHallsFoulSmellingPath a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ FastAbility (HandDiscardCost 2 #any)

instance RunMessage LabyrinthineHallsFoulSmellingPath where
  runMessage msg l@(LabyrinthineHallsFoulSmellingPath attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      atEndOfRound (attrs.ability 1) do
        investigators <- select $ affectsOthers $ NotInvestigator (InvestigatorWithId iid)
        leaveBehind <- if null investigators then pure [iid] else pure investigators
        chooseOrRunOneM iid $ scope "labyrinthineHalls" do
          questionLabeled' "chooseDrawCards"
          targets leaveBehind \iid' -> drawCards iid' (attrs.ability 1) 2
      pure l
    _ -> LabyrinthineHallsFoulSmellingPath <$> liftRunMessage msg attrs
