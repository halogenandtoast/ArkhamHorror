module Arkham.Event.Events.FickleFortune3Spec (spec) where

import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Types (Agenda)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Entities qualified as Entities
import Arkham.Event.Cards qualified as Events
import Arkham.Projection
import TestImport.New

realAgenda :: CardDef -> TestAppT Agenda
realAgenda def = do
  card <- genCard def
  let agenda' = lookupAgenda (AgendaId (toCardCode card)) 1 (toCardId card)
  overTest $ entitiesL . Entities.agendasL %~ insertEntity agenda'
  pure agenda'

spec :: Spec
spec = describe "Fickle Fortune (3)" do
  -- Regression: Fickle Fortune places doom on the current agenda as a *card
  -- effect*, so The Onslaught's forced ability ("when a card effect would place
  -- doom on the agenda, place it on The Captives instead") must trigger. The bug
  -- was that `placeDoomOnAgenda` used the scenario as the source, which fails the
  -- `SourceIsCardEffect` window, so the redirect never fired.
  it "places its doom as a card effect (triggers The Captives redirect)" . gameTest $ \self -> do
    agenda <- realAgenda Agendas.theOnslaught
    captives <- self `putAssetIntoPlay` Assets.theCaptives
    self `playEvent` Events.fickleFortune3
    chooseFirstOption "place doom on agenda"
    useForcedAbility
    field AssetDoom captives `shouldReturn` 1
    agenda.updated.doom `shouldReturn` 0
