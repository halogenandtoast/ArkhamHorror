module Arkham.Agenda.Cards.TheWaterRises (TheWaterRises (..), theWaterRises, theWaterRisesEffect) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Helpers.Scenario
import Arkham.Location.FloodLevel
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Window qualified as Window
import Data.Monoid

newtype TheWaterRises = TheWaterRises AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWaterRises :: AgendaCard TheWaterRises
theWaterRises = agenda (2, A) TheWaterRises Cards.theWaterRises (Static 8)

instance HasAbilities TheWaterRises where
  getAbilities (TheWaterRises a) = [needsAir a 1]

instance RunMessage TheWaterRises where
  runMessage msg a@(TheWaterRises attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach RevealedLocation $ push . (`SetFloodLevel` FullyFlooded)
      cards <- filterCards (cardIs Cards.awakening) <$> scenarioField ScenarioCardsNextToActDeck
      for_ cards obtainCard
      selectEach (EffectWithCardCode Cards.awakening.cardCode) disable
      push $ PlaceNextTo ActDeckTarget [flipCard $ toCard attrs]
      createCardEffect Cards.theWaterRises Nothing attrs attrs
      advanceAgendaDeck attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> TheWaterRises <$> liftRunMessage msg attrs

newtype TheWaterRisesEffect = TheWaterRisesEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWaterRisesEffect :: EffectArgs -> TheWaterRisesEffect
theWaterRisesEffect = cardEffect TheWaterRisesEffect Cards.theWaterRises

instance RunMessage TheWaterRisesEffect where
  runMessage msg e@(TheWaterRisesEffect attrs) = runQueueT $ case msg of
    Do (CheckWindows (filter ((== #when) . Window.windowTiming) -> map Window.windowType -> ws)) -> do
      let
        unRevealLocation (Window.RevealLocation _ lid) = First (Just lid)
        unRevealLocation _ = First Nothing
      for_ (getFirst $ foldMap unRevealLocation ws) $ push . (`SetFloodLevel` FullyFlooded)
      pure e
    _ -> TheWaterRisesEffect <$> liftRunMessage msg attrs
