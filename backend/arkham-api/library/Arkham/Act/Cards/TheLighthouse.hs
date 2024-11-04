module Arkham.Act.Cards.TheLighthouse (TheLighthouse (..), theLighthouse) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Agenda.Types (Field (..))
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheLighthouse = TheLighthouse ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theLighthouse :: ActCard TheLighthouse
theLighthouse = act (1, A) TheLighthouse Cards.theLighthouse (groupClueCost $ PerPlayer 3)

instance RunMessage TheLighthouse where
  runMessage msg a@(TheLighthouse attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      oceiros <-
        createSetAsideEnemyWith
          Enemies.oceirosMarsh
          (location_ "Lighthouse Keeper's Cottage")
          createExhausted
      placeKey oceiros BlueKey
      shuffleSetAsideIntoEncounterDeck [Treacheries.worthHisSalt]
      void $ placeLocationCardInGrid (Pos 2 (-1)) Locations.sunkenGrottoUpperDepths -- lighthouse basement
      push $ AdvanceToAgenda 1 Agendas.unchangingAsTheSea Agenda.A (toSource attrs)
      placeDoomOnAgenda . getSum =<< selectAgg Sum AgendaDoom AnyAgenda
      advanceActDeck attrs
      pure a
    _ -> TheLighthouse <$> liftRunMessage msg attrs
