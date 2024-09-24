module Arkham.Act.Cards.LookingForAnswers (LookingForAnswers (..), lookingForAnswers) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Agenda
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.WakingNightmare.Helpers

newtype LookingForAnswers = LookingForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lookingForAnswers :: ActCard LookingForAnswers
lookingForAnswers = act (1, A) LookingForAnswers Cards.lookingForAnswers (groupClueCost $ PerPlayer 4)

instance RunMessage LookingForAnswers where
  runMessage msg a@(LookingForAnswers attrs) = runQueueT $ case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      step <- getCurrentAgendaStep
      placeSetAsideLocation_ Locations.stairwell
      placeLabeledLocations_ "basementDoor"
        =<< shuffleM
        =<< getSetAsideCardsMatching (CardWithTitle "Basement Door")
      agenda <- selectJust AnyAgenda
      case step of
        1 -> push $ AdvanceAgendaBy agenda #doom
        _ -> makeInfestationTest

      advanceActDeck attrs
      pure a
    _ -> LookingForAnswers <$> liftRunMessage msg attrs
