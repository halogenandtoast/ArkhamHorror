module Arkham.Agenda.Cards.TerrorAtFalconPoint (TerrorAtFalconPoint (..), terrorAtFalconPoint) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Scenarios.ALightInTheFog.Helpers

newtype TerrorAtFalconPoint = TerrorAtFalconPoint AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorAtFalconPoint :: AgendaCard TerrorAtFalconPoint
terrorAtFalconPoint = agenda (4, A) TerrorAtFalconPoint Cards.terrorAtFalconPoint (Static 3)

instance HasModifiersFor TerrorAtFalconPoint where
  getModifiersFor (TerrorAtFalconPoint a) = do
    healthModifier <- perPlayer 2
    modifySelect a (enemyIs Enemies.oceirosMarsh) [HealthModifier healthModifier]

instance RunMessage TerrorAtFalconPoint where
  runMessage msg a@(TerrorAtFalconPoint attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      could <- floodBottommost 4
      push $ if could then RevertAgenda attrs.id else R3
      pure a
    _ -> TerrorAtFalconPoint <$> liftRunMessage msg attrs
