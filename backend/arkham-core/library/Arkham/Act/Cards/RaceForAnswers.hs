module Arkham.Act.Cards.RaceForAnswers
  ( RaceForAnswers(..)
  , raceForAnswers
  ) where

import Arkham.Prelude

import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message

newtype RaceForAnswers = RaceForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

raceForAnswers :: ActCard RaceForAnswers
raceForAnswers = act
  (1, A)
  RaceForAnswers
  Cards.raceForAnswers
  (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage RaceForAnswers where
  runMessage msg a@(RaceForAnswers attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      locations <- selectList $ RevealedLocation <> LocationWithTitle
        "Historical Society"
      playerCount <- getPlayerCount
      a <$ pushAll
        ([ PlaceCluesUpToClueValue location playerCount
         | location <- locations
         ]
        <> [AdvanceActDeck (actDeckId attrs) (toSource attrs)]
        )
    _ -> RaceForAnswers <$> runMessage msg attrs
