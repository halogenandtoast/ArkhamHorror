module Arkham.Types.Act.Cards.RaceForAnswers
  ( RaceForAnswers(..)
  , raceForAnswers
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype RaceForAnswers = RaceForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

raceForAnswers :: ActCard RaceForAnswers
raceForAnswers = act
  (1, A)
  RaceForAnswers
  Cards.raceForAnswers
  (Just $ GroupClueCost (PerPlayer 2) Anywhere)

instance ActRunner env => RunMessage env RaceForAnswers where
  runMessage msg a@(RaceForAnswers attrs) = case msg of
    AdvanceAct aid _ | aid == toId a && onSide B attrs -> do
      locations <- selectList $ RevealedLocation <> LocationWithTitle
        "Historical Society"
      playerCount <- getPlayerCount
      a <$ pushAll
        ([ PlaceCluesUpToClueValue location playerCount
         | location <- locations
         ]
        <> [NextAct aid "03125"]
        )
    _ -> RaceForAnswers <$> runMessage msg attrs
