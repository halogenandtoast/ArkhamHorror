module Arkham.Agenda.Cards.RealitiesInterwoven (
  RealitiesInterwoven (..),
  realitiesInterwoven,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Game.Helpers (perPlayer)
import Arkham.Helpers.Modifiers

newtype RealitiesInterwoven = RealitiesInterwoven AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realitiesInterwoven :: AgendaCard RealitiesInterwoven
realitiesInterwoven = agenda (3, A) RealitiesInterwoven Cards.realitiesInterwoven (Static 11)

instance HasModifiersFor RealitiesInterwoven where
  getModifiersFor target (RealitiesInterwoven attrs) | attrs `is` target = do
    n <- perPlayer 2
    pure $ toModifiers attrs [DoomThresholdModifier n]
  getModifiersFor _ _ = pure []

instance RunMessage RealitiesInterwoven where
  runMessage msg a@(RealitiesInterwoven attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R2
      pure a
    _ -> RealitiesInterwoven <$> runMessage msg attrs
