module Arkham.Event.Events.ThePaintedWorld (thePaintedWorld, thePaintedWorldEffect) where

import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Cost
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (withAdditionalResources)
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
    InHand _ (BeforeCardCost iid actionStatus windows' cardId) | cardId == toCardId attrs -> do
      mods <- getModifiers (toCardId attrs)
      let reduction = sum [n | ReduceCostOf _ n <- mods]
      candidates <- fieldMap InvestigatorCardsUnderneath (filterCards (NonExceptional <> #event)) iid
      let playableWindows = nub $ mkWhen (DuringTurn iid) : windows'
      playableCards <- withAdditionalResources iid reduction do
        filterM (getIsPlayable iid attrs (UnpaidCost actionStatus) playableWindows) candidates

      chooseOneM iid $ targets playableCards $ handleTarget iid attrs
      pure e
    InHand _ (HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid)) -> do
      createCardEffect Cards.thePaintedWorld Nothing (toCardId attrs) (toCardId attrs)
      choice <- fetchCard cid
      let
        choiceDef = toCardDef choice
        choiceAsCard =
          (lookupPlayerCard choiceDef $ toCardId attrs)
            { pcOriginalCardCode = toCardCode attrs
            , pcCustomizations = choice.customizations
            , pcMutated = choice.mutated
            , pcOwner = Just iid
            }
      replaceCard (toCardId attrs) (PlayerCard choiceAsCard)
      pure e
    _ -> ThePaintedWorld <$> liftRunMessage msg attrs

newtype ThePaintedWorldEffect = ThePaintedWorldEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorldEffect :: EffectArgs -> ThePaintedWorldEffect
thePaintedWorldEffect = cardEffect ThePaintedWorldEffect Cards.thePaintedWorld

instance HasModifiersFor ThePaintedWorldEffect where
  getModifiersFor (ThePaintedWorldEffect a) = do
    case a.target of
      CardIdTarget cardId -> do
        modifySelect a (EventWithCardId cardId) [RemoveFromGameInsteadOfDiscard]
        modified_ a a.target [RemoveFromGameInsteadOfDiscard]
      _ -> pure ()
