module Arkham.Event.Cards.SleightOfHand (sleightOfHand, sleightOfHandEffect, SleightOfHand (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Cost.Status qualified as Cost
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype SleightOfHand = SleightOfHand EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleightOfHand :: EventCard SleightOfHand
sleightOfHand = event SleightOfHand Cards.sleightOfHand

instance RunMessage SleightOfHand where
  runMessage msg e@(SleightOfHand attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      cards <- select $ PlayableCard Cost.PaidCost $ inHandOf iid <> basic #item
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel
            (toCardId card)
            [ PutCardIntoPlay iid card (Just $ toTarget attrs) NoPayment windows'
            , createCardEffect Cards.sleightOfHand Nothing attrs card
            ]
          | card <- cards
          ]
      pure e
    _ -> SleightOfHand <$> runMessage msg attrs

newtype SleightOfHandEffect = SleightOfHandEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleightOfHandEffect :: EffectArgs -> SleightOfHandEffect
sleightOfHandEffect = cardEffect SleightOfHandEffect Cards.sleightOfHand

instance RunMessage SleightOfHandEffect where
  runMessage msg e@(SleightOfHandEffect attrs) = case msg of
    EndTurn _ -> do
      case attrs.target of
        CardIdTarget cid -> do
          mAid <- selectOne (AssetWithCardId cid)
          for_ mAid \aid -> do
            mController <- selectAssetController aid
            for_ mController \controllerId ->
              push (ReturnToHand controllerId (AssetTarget aid))
        _ -> pure ()
      push $ disable attrs
      pure e
    _ -> SleightOfHandEffect <$> runMessage msg attrs
