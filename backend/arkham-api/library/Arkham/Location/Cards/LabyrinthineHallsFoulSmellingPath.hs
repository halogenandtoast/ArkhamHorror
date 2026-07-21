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
      $ restricted a 1 (Here <> exists (not_ You))
      $ FastAbility (HandDiscardCost 2 #any)

instance RunMessage LabyrinthineHallsFoulSmellingPath where
  runMessage msg l@(LabyrinthineHallsFoulSmellingPath attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      atEndOfRound (attrs.ability 1) $ doStep 1 msg
      pure l
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      investigators <- select $ affectsOthersKnown iid $ not_ (InvestigatorWithId iid)
      for_ (nonEmpty investigators) \others ->
        chooseOrRunOneM iid $ scope "labyrinthineHalls" do
          questionLabeled' "chooseDrawCards"
          targets (toList others) \iid' -> drawCards iid' (attrs.ability 1) 2
      pure l
    _ -> LabyrinthineHallsFoulSmellingPath <$> liftRunMessage msg attrs
