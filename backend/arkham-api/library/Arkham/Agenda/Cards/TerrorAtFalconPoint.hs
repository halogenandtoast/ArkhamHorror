module Arkham.Agenda.Cards.TerrorAtFalconPoint (
  TerrorAtFalconPoint (..),
  terrorAtFalconPoint,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype TerrorAtFalconPoint = TerrorAtFalconPoint AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

terrorAtFalconPoint :: AgendaCard TerrorAtFalconPoint
terrorAtFalconPoint = agenda (4, A) TerrorAtFalconPoint Cards.terrorAtFalconPoint (Static 12)

instance HasModifiersFor TerrorAtFalconPoint where
  getModifiersFor (EnemyTarget eid) (TerrorAtFalconPoint a) = do
    isOcieros <- eid <=~> enemyIs Enemies.oceirosMarsh
    healthModifier <- perPlayer 2
    toModifiers a [HealthModifier healthModifier | isOcieros]
  getModifiersFor _ _ = pure []

instance RunMessage TerrorAtFalconPoint where
  runMessage msg a@(TerrorAtFalconPoint attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TerrorAtFalconPoint <$> liftRunMessage msg attrs
