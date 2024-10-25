module Arkham.Agenda.Cards.FogOnTheBay (FogOnTheBay (..), fogOnTheBay) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as Act
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype FogOnTheBay = FogOnTheBay AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fogOnTheBay :: AgendaCard FogOnTheBay
fogOnTheBay =
  agendaWith (1, A) FogOnTheBay Cards.fogOnTheBay (Static 4)
    $ removeDoomMatchersL
    %~ (\rdm -> rdm {removeDoomAgendas = NotAgenda AnyAgenda})

-- Ability is no-op
instance HasAbilities FogOnTheBay where
  getAbilities (FogOnTheBay a) = [mkAbility a 1 $ forced $ AgendaAdvances #when (AgendaWithId a.id)]

instance RunMessage FogOnTheBay where
  runMessage msg a@(FogOnTheBay attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      oceiros <- createSetAsideEnemy Enemies.oceirosMarsh =<< getLead
      placeKey oceiros BlueKey
      shuffleSetAsideIntoEncounterDeck [Treacheries.worthHisSalt]
      eachInvestigator $ discardAllClues attrs
      void $ placeLocationCardInGrid (Pos 2 (-1)) Locations.sunkenGrottoUpperDepths -- lighthouse basement
      push $ AdvanceToAct 1 Acts.findingThePath Act.A (toSource attrs)
      advanceAgendaDeck attrs
      placeDoomOnAgenda attrs.doom
      pure a
    _ -> FogOnTheBay <$> liftRunMessage msg attrs
