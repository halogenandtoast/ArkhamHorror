module Arkham.Event.Events.SleightOfHand (sleightOfHand, sleightOfHandEffect) where

import Arkham.Cost
import Arkham.Cost.Status qualified as Cost
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Query (selectAssetController)
import Arkham.Matcher
import Arkham.Taboo

newtype SleightOfHand = SleightOfHand EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleightOfHand :: EventCard SleightOfHand
sleightOfHand = event SleightOfHand Cards.sleightOfHand

instance RunMessage SleightOfHand where
  runMessage msg e@(SleightOfHand attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      let
        tabooMatcher
          | tabooed TabooList18 attrs = (CardWithMaxLevel 3 <>)
          | tabooed TabooList15 attrs = (CardFillsLessSlots 2 #hand <>)
          | otherwise = id
      cards <-
        select $ PlayableCard Cost.PaidCost $ inHandOf ForPlay iid <> basic (tabooMatcher #item)
      chooseTargetM iid cards \card -> do
        push $ PutCardIntoPlay iid card (Just $ toTarget attrs) NoPayment windows'
        createCardEffect Cards.sleightOfHand Nothing attrs card
      pure e
    _ -> SleightOfHand <$> liftRunMessage msg attrs

newtype SleightOfHandEffect = SleightOfHandEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleightOfHandEffect :: EffectArgs -> SleightOfHandEffect
sleightOfHandEffect = cardEffect SleightOfHandEffect Cards.sleightOfHand

instance RunMessage SleightOfHandEffect where
  runMessage msg e@(SleightOfHandEffect attrs) = runQueueT $ case msg of
    EndTurn _ -> do
      case attrs.target of
        CardIdTarget cid -> do
          selectOne (AssetWithCardId cid) >>= traverse_ \aid -> selectAssetController aid >>= traverse_ (`returnToHand` aid)
        _ -> pure ()
      disableReturn e
    _ -> SleightOfHandEffect <$> liftRunMessage msg attrs
