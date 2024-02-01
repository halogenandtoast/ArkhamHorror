module Arkham.Act.Cards.LookingForAnswers (
  LookingForAnswers (..),
  lookingForAnswers,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Helpers.Agenda
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenarios.WakingNightmare.Helpers

newtype LookingForAnswers = LookingForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

lookingForAnswers :: ActCard LookingForAnswers
lookingForAnswers = act (1, A) LookingForAnswers Cards.lookingForAnswers (groupClueCost $ PerPlayer 4)

instance RunMessage LookingForAnswers where
  runMessage msg a@(LookingForAnswers attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      step <- getCurrentAgendaStep
      placeStairwell <- placeSetAsideLocation_ Locations.stairwell
      placeBasementDoors <-
        placeLabeledLocations_ "basementDoor"
          =<< shuffleM
          =<< getSetAsideCardsMatching (CardWithTitle "Basement Door")
      agenda <- selectJust AnyAgenda
      agendaMsg <- case step of
        1 -> pure $ AdvanceAgenda agenda
        _ -> makeInfestationTest

      pushAll $ placeStairwell : placeBasementDoors <> [agendaMsg, advanceActDeck attrs]

      pure a
    _ -> LookingForAnswers <$> runMessage msg attrs
