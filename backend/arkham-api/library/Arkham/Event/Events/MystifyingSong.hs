module Arkham.Event.Events.MystifyingSong (
  mystifyingSong,
  mystifyingSongEffect,
  MystifyingSong (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher.Agenda
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype MystifyingSong = MystifyingSong EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mystifyingSong :: EventCard MystifyingSong
mystifyingSong =
  event MystifyingSong Cards.mystifyingSong

isAfterAgendaWouldAdvanceWindow :: Window -> Bool
isAfterAgendaWouldAdvanceWindow ((windowType -> Window.AgendaWouldAdvance _ _)) = True
isAfterAgendaWouldAdvanceWindow _ = False

instance RunMessage MystifyingSong where
  runMessage msg e@(MystifyingSong attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      popMessageMatching_ $ \case
        Do AdvanceAgendaIfThresholdSatisfied -> True
        _ -> False
      popMessageMatching_ $ \case
        CheckWindows ws -> any isAfterAgendaWouldAdvanceWindow ws
        Do (CheckWindows ws) -> any isAfterAgendaWouldAdvanceWindow ws
        _ -> False
      push =<< createCardEffect Cards.mystifyingSong Nothing attrs iid
      pure e
    _ -> MystifyingSong <$> runMessage msg attrs

newtype MystifyingSongEffect = MystifyingSongEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mystifyingSongEffect :: EffectArgs -> MystifyingSongEffect
mystifyingSongEffect = cardEffect MystifyingSongEffect Cards.mystifyingSong

instance HasModifiersFor MystifyingSongEffect where
  getModifiersFor (MystifyingSongEffect a) =
    modifySelect a AnyAgenda [CannotBeAdvancedByDoomThreshold]

instance RunMessage MystifyingSongEffect where
  runMessage msg e@(MystifyingSongEffect attrs@EffectAttrs {..}) = case msg of
    EndPhase -> do
      push (DisableEffect effectId)
      pure e
    _ -> MystifyingSongEffect <$> runMessage msg attrs
