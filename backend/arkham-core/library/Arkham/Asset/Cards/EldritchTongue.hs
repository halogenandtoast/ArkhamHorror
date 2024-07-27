module Arkham.Asset.Cards.EldritchTongue (eldritchTongue, eldritchTongueEffect, EldritchTongue (..)) where

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Cost
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher
import Arkham.Strategy

newtype EldritchTongue = EldritchTongue AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchTongue :: AssetCard EldritchTongue
eldritchTongue = asset EldritchTongue Cards.eldritchTongue

instance RunMessage EldritchTongue where
  runMessage msg a@(EldritchTongue attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      createCardEffect Cards.eldritchTongue Nothing attrs iid
      pure a
    _ -> EldritchTongue <$> liftRunMessage msg attrs

newtype EldritchTongueEffect = EldritchTongueEffect EffectAttrs
  deriving anyclass (IsEffect, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchTongueEffect :: EffectArgs -> EldritchTongueEffect
eldritchTongueEffect = cardEffect EldritchTongueEffect Cards.eldritchTongue

instance HasModifiersFor EldritchTongueEffect where
  getModifiersFor (InvestigatorTarget iid) (EldritchTongueEffect a) | isTarget iid a.target = do
    maybeModified a do
      liftGuardM
        $ selectAny
        $ PlayableCard (UnpaidCost NeedsAction)
        $ inDiscardOf iid
        <> #event
        <> #parley
      x <- hoistMaybe a.source.asset
      liftGuardM $ x <=~> AssetWithUseCount Charge (atLeast 1)
      pure
        [ GiveAdditionalAction
            $ AdditionalAction "Eldritch Tongue" (toSource a)
            $ EffectAction "Use Eldritch Tongue to play parley event"
            $ toId a
        ]
  getModifiersFor _ _ = pure []

instance RunMessage EldritchTongueEffect where
  runMessage msg e@(EldritchTongueEffect attrs) = runQueueT $ case msg of
    UseEffectAction iid eid ws | eid == toId attrs -> do
      cards <- select $ PlayableCard (UnpaidCost NeedsAction) $ inDiscardOf iid <> #event <> #parley
      for_ attrs.source.asset \aid -> do
        focusCards cards \unfocus -> do
          chooseOne
            iid
            [ targetLabel
              card
              [ unfocus
              , AddToHand iid [card]
              , Msg.eventModifiers
                  attrs.source
                  card
                  [SetAfterPlay AbsoluteRemoveThisFromGame, AdditionalCost $ UseCost (AssetWithId aid) Charge 1]
              , PayCardCost iid card ws
              ]
            | card <- cards
            ]

      pure e
    RemovedFromPlay source | isSource source attrs.source -> do
      disableReturn e
    _ -> EldritchTongueEffect <$> runMessage msg attrs
