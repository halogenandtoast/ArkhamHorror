module Arkham.Treachery.Cards.PushedIntoTheBeyond (
  PushedIntoTheBeyond (..),
  pushedIntoTheBeyond,
  pushedIntoTheBeyondEffect,
) where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Exception
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PushedIntoTheBeyond = PushedIntoTheBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedIntoTheBeyond :: TreacheryCard PushedIntoTheBeyond
pushedIntoTheBeyond = treachery PushedIntoTheBeyond Cards.pushedIntoTheBeyond

instance RunMessage PushedIntoTheBeyond where
  runMessage msg t@(PushedIntoTheBeyond attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      choices <-
        selectWithField AssetCardCode
          $ assetControlledBy iid
          <> AssetNonStory
          <> AssetCanLeavePlayByNormalMeans
      when (notNull choices) do
        chooseOneM iid do
          for_ choices \(aid, cardCode) ->
            targeting aid do
              shuffleIntoDeck iid aid
              createCardEffect Cards.pushedIntoTheBeyond (Just (EffectCardCodes [cardCode])) attrs iid
      pure t
    _ -> PushedIntoTheBeyond <$> liftRunMessage msg attrs

newtype PushedIntoTheBeyondEffect = PushedIntoTheBeyondEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedIntoTheBeyondEffect :: EffectArgs -> PushedIntoTheBeyondEffect
pushedIntoTheBeyondEffect = cardEffect PushedIntoTheBeyondEffect Cards.pushedIntoTheBeyond

instance RunMessage PushedIntoTheBeyondEffect where
  runMessage msg e@(PushedIntoTheBeyondEffect attrs) = runQueueT $ case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == attrs.id -> do
      push $ DiscardTopOfDeck iid 3 attrs.source (Just $ EffectTarget eid)
      pure e
    DiscardedTopOfDeck iid cards _ (EffectTarget eid) | eid == attrs.id -> do
      case attrs.metadata of
        Just (EffectCardCodes [x]) -> do
          when (x `elem` map (cdCardCode . toCardDef) cards) (assignHorror iid attrs.source 2)
        _ -> throwIO (InvalidState "Must have one card as the target")
      pure e
    _ -> PushedIntoTheBeyondEffect <$> liftRunMessage msg attrs
