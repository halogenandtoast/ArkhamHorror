module Arkham.Agenda.Cards.Awakening (Awakening (..), awakening, awakeningEffect) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Matcher
import Arkham.Window qualified as Window
import Data.Monoid

newtype Awakening = Awakening AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

awakening :: AgendaCard Awakening
awakening = agenda (1, A) Awakening Cards.awakening (Static 7)

instance HasAbilities Awakening where
  getAbilities (Awakening a) = [needsAir a 1]

instance RunMessage Awakening where
  runMessage msg a@(Awakening attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach RevealedLocation $ push . IncreaseFloodLevel
      push $ PlaceNextTo ActDeckTarget [flipCard $ toCard attrs]
      createCardEffect Cards.awakening Nothing attrs attrs
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> Awakening <$> liftRunMessage msg attrs

newtype AwakeningEffect = AwakeningEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

awakeningEffect :: EffectArgs -> AwakeningEffect
awakeningEffect = cardEffect AwakeningEffect Cards.awakening

instance RunMessage AwakeningEffect where
  runMessage msg e@(AwakeningEffect attrs) = runQueueT $ case msg of
    Do (CheckWindows (filter ((== #when) . Window.windowTiming) -> map Window.windowType -> ws)) -> do
      let
        unRevealLocation (Window.RevealLocation _ lid) = First (Just lid)
        unRevealLocation _ = First Nothing
      for_ (getFirst $ foldMap unRevealLocation ws) $ push . IncreaseFloodLevel
      pure e
    _ -> AwakeningEffect <$> liftRunMessage msg attrs
