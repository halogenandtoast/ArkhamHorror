module Arkham.Event.Cards.AChanceEncounter (
  aChanceEncounter,
  aChanceEncounterEffect,
  AChanceEncounter (..),
) where

import Arkham.Capability
import Arkham.Cost
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Effect qualified as Msg
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher

newtype AChanceEncounter = AChanceEncounter EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounter :: EventCard AChanceEncounter
aChanceEncounter = event AChanceEncounter Cards.aChanceEncounter

instance RunMessage AChanceEncounter where
  runMessage msg e@(AChanceEncounter attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == attrs.id -> do
      discards <- select $ #ally <> InDiscardOf (affectsOthers can.have.cards.leaveDiscard)
      focusCards discards \unfocus -> do
        chooseOne
          iid
          [ targetLabel
            card
            [ PutCardIntoPlay iid card Nothing NoPayment windows'
            , Msg.createCardEffect Cards.aChanceEncounter Nothing attrs card
            ]
          | card <- discards
          ]
        push unfocus
      pure e
    _ -> AChanceEncounter <$> liftRunMessage msg attrs

newtype AChanceEncounterEffect = AChanceEncounterEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounterEffect :: EffectArgs -> AChanceEncounterEffect
aChanceEncounterEffect = cardEffect AChanceEncounterEffect Cards.aChanceEncounter

instance RunMessage AChanceEncounterEffect where
  runMessage msg e@(AChanceEncounterEffect attrs) = runQueueT $ case msg of
    EndRoundWindow -> case attrs.target of
      CardIdTarget cardId -> do
        -- TODO: we should include the investigator id here
        -- currently we can only get the card owner
        push $ Msg.toDiscard attrs.source cardId
        disableReturn e
      _ -> error "Wrong target type"
    _ -> AChanceEncounterEffect <$> liftRunMessage msg attrs
