module Arkham.Event.Cards.AChanceEncounter (
  aChanceEncounter,
  aChanceEncounterEffect,
  AChanceEncounter (..),
) where

import Arkham.Prelude

import Arkham.Capability
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait

newtype AChanceEncounter = AChanceEncounter EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounter :: EventCard AChanceEncounter
aChanceEncounter = event AChanceEncounter Cards.aChanceEncounter

instance RunMessage AChanceEncounter where
  runMessage msg e@(AChanceEncounter attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      investigatorIds <- select $ affectsOthers can.have.cards.leaveDiscard
      discards <- concatMapM (fieldMap InvestigatorDiscard (map PlayerCard)) investigatorIds
      let filteredDiscards = filter (elem Ally . toTraits) discards
      player <- getPlayer iid
      pushAll
        [ FocusCards filteredDiscards
        , chooseOne
            player
            [ targetLabel
              (toCardId card)
              [ PutCardIntoPlay iid card Nothing NoPayment windows'
              , RemoveFromDiscard iid (toCardId card)
              , CreateEffect "02270" Nothing (toSource attrs) (toTarget $ toCardId card)
              ]
            | card <- filteredDiscards
            ]
        , UnfocusCards
        ]
      pure e
    _ -> AChanceEncounter <$> runMessage msg attrs

newtype AChanceEncounterEffect = AChanceEncounterEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounterEffect :: EffectArgs -> AChanceEncounterEffect
aChanceEncounterEffect = cardEffect AChanceEncounterEffect Cards.aChanceEncounter

instance RunMessage AChanceEncounterEffect where
  runMessage msg e@(AChanceEncounterEffect attrs) = case msg of
    EndRoundWindow -> case attrs.target of
      CardIdTarget cardId -> do
        -- TODO: we should include the investigator id here
        -- currently we can only get the card owner
        pushAll
          [toDiscard attrs.source cardId, disable attrs]
        pure e
      _ -> error "Wrong target type"
    _ -> AChanceEncounterEffect <$> runMessage msg attrs
