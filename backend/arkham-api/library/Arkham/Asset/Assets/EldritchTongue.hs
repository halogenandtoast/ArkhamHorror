module Arkham.Asset.Assets.EldritchTongue (eldritchTongue, eldritchTongueEffect, EldritchTongue (..)) where

import Arkham.Action.Additional
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Cost
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
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
  getModifiersFor (EldritchTongueEffect a) = case a.target of
    InvestigatorTarget iid -> maybeModified_ a iid do
      liftGuardM
        $ selectAny
        $ PlayableCard (UnpaidCost NeedsAction)
        $ inDiscardOf iid
        <> #event
        <> oneOf [#parley, basic (CardTaggedWith "parley")]
      x <- hoistMaybe a.source.asset
      liftGuardM $ x <=~> AssetWithUseCount Charge (atLeast 1)
      pure
        [ GiveAdditionalAction
            $ AdditionalAction "Eldritch Tongue" (toSource a)
            $ EffectAction "Use Eldritch Tongue to play parley event"
            $ toId a
        ]
    _ -> pure mempty

instance RunMessage EldritchTongueEffect where
  runMessage msg e@(EldritchTongueEffect attrs) = runQueueT $ case msg of
    UseEffectAction iid eid ws | eid == toId attrs -> do
      cards <-
        select
          $ PlayableCard (UnpaidCost NeedsAction)
          $ inDiscardOf iid
          <> #event
          <> oneOf [#parley, basic (CardTaggedWith "parley")]
      for_ attrs.source.asset \aid -> do
        focusCards cards \unfocus -> do
          chooseOneM iid do
            targets cards \card -> do
              push unfocus
              eventModifiers
                attrs.source
                card
                [ SetAfterPlay AbsoluteRemoveThisFromGame
                , AdditionalCost $ ActionCost 1 <> UseCost (AssetWithId aid) Charge 1
                ]
              playCardPayingCostWithWindows iid card ws
              disable attrs
              createCardEffect Cards.eldritchTongue Nothing attrs.source iid
      pure e
    RemovedFromPlay source | isSource source attrs.source -> disableReturn e
    _ -> EldritchTongueEffect <$> liftRunMessage msg attrs
