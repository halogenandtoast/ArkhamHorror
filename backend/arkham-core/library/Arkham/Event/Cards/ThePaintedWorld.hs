module Arkham.Event.Cards.ThePaintedWorld (thePaintedWorld, thePaintedWorldEffect, ThePaintedWorld (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype ThePaintedWorld = ThePaintedWorld EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorld :: EventCard ThePaintedWorld
thePaintedWorld = eventWith ThePaintedWorld Cards.thePaintedWorld $ afterPlayL .~ RemoveThisFromGame

instance RunMessage ThePaintedWorld where
  runMessage msg e@(ThePaintedWorld attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      candidates <-
        fieldMap
          InvestigatorCardsUnderneath
          (filter (`cardMatch` (NonExceptional <> Matcher.EventCard)))
          iid
      let
        playableWindows = nub $ mkWindow Timing.When (DuringTurn iid) : windows'
      playableCards <-
        filterM
          (getIsPlayable iid (toSource attrs) (UnpaidCost NoAction) playableWindows)
          candidates
      push
        $ InitiatePlayCardAsChoose
          iid
          (toCard attrs)
          playableCards
          [ createCardEffect
              Cards.thePaintedWorld
              Nothing
              (CardSource $ toCard attrs)
              (CardIdTarget $ toCardId attrs)
          ]
          LeaveChosenCard
          NoPayment
          playableWindows
          True
      pure e
    _ -> ThePaintedWorld <$> runMessage msg attrs

newtype ThePaintedWorldEffect = ThePaintedWorldEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorldEffect :: EffectArgs -> ThePaintedWorldEffect
thePaintedWorldEffect = cardEffect ThePaintedWorldEffect Cards.thePaintedWorld

instance HasModifiersFor ThePaintedWorldEffect where
  getModifiersFor (EventTarget eid) (ThePaintedWorldEffect a@EffectAttrs {..}) = do
    cardId <- field EventCardId eid
    pure $ toModifiers a [RemoveFromGameInsteadOfDiscard | toTarget cardId == effectTarget]
  getModifiersFor (CardIdTarget cardId) (ThePaintedWorldEffect a@EffectAttrs {..}) = do
    -- Mainly used for cards like Crystallizer of Dreams that work with After you play effects
    pure $ toModifiers a [RemoveFromGameInsteadOfDiscard | toTarget cardId == effectTarget]
  getModifiersFor _ _ = pure []

instance RunMessage ThePaintedWorldEffect where
  runMessage msg (ThePaintedWorldEffect attrs) =
    ThePaintedWorldEffect <$> runMessage msg attrs
