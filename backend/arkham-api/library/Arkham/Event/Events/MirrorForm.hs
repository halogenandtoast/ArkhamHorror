module Arkham.Event.Events.MirrorForm (mirrorForm, mirrorFormEffect) where

import Arkham.Deck qualified as Deck
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Query (selectAssetController)
import Arkham.Matcher

newtype MirrorForm = MirrorForm EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirrorForm :: EventCard MirrorForm
mirrorForm = event MirrorForm Cards.mirrorForm

instance RunMessage MirrorForm where
  runMessage msg e@(MirrorForm attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- select $ inHandOf NotForPlay iid <> basic (oneOf [#spell, #charm]) <> #asset
      focusCards cards do
        chooseTargetM iid cards \card -> do
          unfocusCards
          putCardIntoPlay iid card
          createCardEffect Cards.mirrorForm Nothing attrs card
      pure e
    _ -> MirrorForm <$> liftRunMessage msg attrs

newtype MirrorFormEffect = MirrorFormEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirrorFormEffect :: EffectArgs -> MirrorFormEffect
mirrorFormEffect = cardEffect MirrorFormEffect Cards.mirrorForm

instance RunMessage MirrorFormEffect where
  runMessage msg e@(MirrorFormEffect attrs) = runQueueT $ case msg of
    EndRound -> do
      case attrs.target of
        CardIdTarget cid ->
          selectOne (AssetWithCardId cid) >>= traverse_ \aid ->
            selectAssetController aid >>= traverse_ \iid ->
              push $ ShuffleIntoDeck (Deck.InvestigatorDeck iid) (AssetTarget aid)
        _ -> pure ()
      disableReturn e
    _ -> MirrorFormEffect <$> liftRunMessage msg attrs
