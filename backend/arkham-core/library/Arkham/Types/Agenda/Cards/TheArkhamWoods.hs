module Arkham.Types.Agenda.Cards.TheArkhamWoods where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype TheArkhamWoods = TheArkhamWoods AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theArkhamWoods :: AgendaCard TheArkhamWoods
theArkhamWoods = agenda (1, A) TheArkhamWoods Cards.theArkhamWoods (Static 4)

instance AgendaRunner env => RunMessage env TheArkhamWoods where
  runMessage msg a@(TheArkhamWoods attrs) = case msg of
    AdvanceAgenda aid | aid == toId a && agendaSequence attrs == Agenda 1 B ->
      a <$ push
        (Run
          [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (AgendaSource aid)
            (CardWithType EnemyType <> CardWithTrait Monster)
          ]
        )
    RequestedEncounterCard source mcard | isSource attrs source -> case mcard of
      Nothing -> a <$ push (NextAgenda (toId a) "01144")
      Just card -> do
        mainPathId <- getJustLocationIdByName "Main Path"
        a <$ pushAll
          [ SpawnEnemyAt (EncounterCard card) mainPathId
          , PlaceDoom (CardIdTarget $ toCardId card) 1
          , NextAgenda (toId a) "01144"
          ]
    _ -> TheArkhamWoods <$> runMessage msg attrs
