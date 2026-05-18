module Arkham.Agenda.Cards.ShadowsDeepenSpec (spec) where

import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence (AgendaSequence (Sequence), AgendaSide (A, B))
import Arkham.Agenda.Types (Agenda, AgendaAttrs (..))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Entities qualified as Entities
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

-- | Regression for the FAQ ruling that side-A constant/forced abilities remain
-- in effect while resolving the b-side of an agenda. Shadows Deepen prints its
-- forced ability on side A; its b-side spawns the Hunting Horror, so the
-- forced ability must still fire during that spawn.
realAgendaOnSide :: CardDef -> AgendaSide -> TestAppT Agenda
realAgendaOnSide def side = do
  card <- genCard def
  let agendaId' = AgendaId (toCardCode card)
      agenda' =
        overAttrs
          (\attrs -> attrs {agendaSequence = Sequence 2 side})
          (lookupAgenda agendaId' 1 (toCardId card))
  overTest $ entitiesL . Entities.agendasL %~ insertEntity agenda'
  pure agenda'

spec :: Spec
spec = describe "Shadows Deepen" do
  context "side-A forced ability (attach Shadow Spawned when Hunting Horror enters play)" do
    it "fires while the agenda is on side A" . gameTest $ \_self -> do
      _ <- realAgendaOnSide Agendas.shadowsDeepen A
      location <- testLocation
      huntingHorror <- testEnemyWithDef Enemies.huntingHorror id
      huntingHorror `spawnAt` location
      useForcedAbility
      assertAny
        $ treacheryIs Treacheries.shadowSpawned
        <> TreacheryIsAttachedTo (toTarget huntingHorror)

    it "still fires while the agenda is on side B (during b-side resolution)" . gameTest $ \_self -> do
      _ <- realAgendaOnSide Agendas.shadowsDeepen B
      location <- testLocation
      huntingHorror <- testEnemyWithDef Enemies.huntingHorror id
      huntingHorror `spawnAt` location
      useForcedAbility
      assertAny
        $ treacheryIs Treacheries.shadowSpawned
        <> TreacheryIsAttachedTo (toTarget huntingHorror)
