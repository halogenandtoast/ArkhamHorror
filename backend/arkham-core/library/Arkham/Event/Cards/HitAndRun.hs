module Arkham.Event.Cards.HitAndRun (hitAndRun, hitAndRunEffect, HitAndRun (..)) where

import Arkham.Cost
import Arkham.Cost.Status qualified as Cost
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Query (selectAssetController)
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype HitAndRun = HitAndRun EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hitAndRun :: EventCard HitAndRun
hitAndRun = event HitAndRun Cards.hitAndRun

instance RunMessage HitAndRun where
  runMessage msg e@(HitAndRun attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      cards <- select $ PlayableCard Cost.PaidCost $ inHandOf iid <> basic (#ally <> #asset)
      chooseOne
        iid
        [ targetLabel
          card
          [ PutCardIntoPlay iid card (Just $ toTarget attrs) NoPayment windows'
          , Msg.createCardEffect Cards.hitAndRun Nothing attrs card
          ]
        | card <- cards
        ]
      pure e
    _ -> HitAndRun <$> liftRunMessage msg attrs

newtype HitAndRunEffect = HitAndRunEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hitAndRunEffect :: EffectArgs -> HitAndRunEffect
hitAndRunEffect = cardEffect HitAndRunEffect Cards.hitAndRun

instance RunMessage HitAndRunEffect where
  runMessage msg e@(HitAndRunEffect attrs) = runQueueT $ case msg of
    EndTurn _ -> do
      case attrs.target of
        CardIdTarget cid -> do
          selectOne (AssetWithCardId cid) >>= traverse_ \aid -> selectAssetController aid >>= traverse_ \controller -> returnToHand controller aid
        _ -> pure ()
      disableReturn e
    _ -> HitAndRunEffect <$> liftRunMessage msg attrs
