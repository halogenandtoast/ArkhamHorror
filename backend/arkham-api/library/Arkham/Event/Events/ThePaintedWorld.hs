module Arkham.Event.Events.ThePaintedWorld (thePaintedWorld, thePaintedWorldEffect) where

import Arkham.Card
import Arkham.Cost
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Effect qualified as Msg
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Playable
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Projection
import Arkham.Strategy
import Arkham.Window

newtype ThePaintedWorld = ThePaintedWorld EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorld :: EventCard ThePaintedWorld
thePaintedWorld = eventWith ThePaintedWorld Cards.thePaintedWorld $ afterPlayL .~ RemoveThisFromGame

instance RunMessage ThePaintedWorld where
  runMessage msg e@(ThePaintedWorld attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid (is attrs -> True) _ windows' _ -> do
      candidates <- fieldMap InvestigatorCardsUnderneath (filterCards (NonExceptional <> #event)) iid
      let playableWindows = nub $ mkWhen (DuringTurn iid) : windows'
      playableCards <- filterM (getIsPlayable iid attrs (UnpaidCost NoAction) playableWindows) candidates
      enabled <- Msg.createCardEffect Cards.thePaintedWorld Nothing (toCardId attrs) (toCardId attrs)

      push
        $ InitiatePlayCardAsChoose
          iid
          (toCard attrs)
          playableCards
          [enabled]
          LeaveChosenCard
          NoPayment
          playableWindows
          True
      pure e
    _ -> ThePaintedWorld <$> liftRunMessage msg attrs

newtype ThePaintedWorldEffect = ThePaintedWorldEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorldEffect :: EffectArgs -> ThePaintedWorldEffect
thePaintedWorldEffect = cardEffect ThePaintedWorldEffect Cards.thePaintedWorld

instance HasModifiersFor ThePaintedWorldEffect where
  getModifiersFor (ThePaintedWorldEffect a) = do
    case a.target of
      CardIdTarget cardId -> do
        events <- modifySelect a (EventWithCardId cardId) [RemoveFromGameInsteadOfDiscard]
        cards <- modified_ a a.target [RemoveFromGameInsteadOfDiscard]
        pure $ events <> cards
      _ -> pure mempty

instance RunMessage ThePaintedWorldEffect where
  runMessage msg (ThePaintedWorldEffect attrs) = ThePaintedWorldEffect <$> runMessage msg attrs
