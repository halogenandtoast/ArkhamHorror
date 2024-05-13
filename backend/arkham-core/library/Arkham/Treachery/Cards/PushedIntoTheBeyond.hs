module Arkham.Treachery.Cards.PushedIntoTheBeyond (PushedIntoTheBeyond (..), pushedIntoTheBeyond, pushedIntoTheBeyondEffect) where

import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Effect.Runner
import Arkham.Exception
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PushedIntoTheBeyond = PushedIntoTheBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedIntoTheBeyond :: TreacheryCard PushedIntoTheBeyond
pushedIntoTheBeyond = treachery PushedIntoTheBeyond Cards.pushedIntoTheBeyond

instance RunMessage PushedIntoTheBeyond where
  runMessage msg t@(PushedIntoTheBeyond attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      targets <-
        selectWithField AssetCardCode
          $ assetControlledBy iid
          <> AssetNonStory
          <> AssetCanLeavePlayByNormalMeans
      player <- getPlayer iid
      pushWhen (notNull targets)
        $ chooseOne
          player
          [ targetLabel
            aid
            [ ShuffleIntoDeck (Deck.InvestigatorDeck iid) (AssetTarget aid)
            , createCardEffect Cards.pushedIntoTheBeyond (Just (EffectCardCodes [cardCode])) attrs iid
            ]
          | (aid, cardCode) <- targets
          ]
      pure t
    _ -> PushedIntoTheBeyond <$> runMessage msg attrs

newtype PushedIntoTheBeyondEffect = PushedIntoTheBeyondEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedIntoTheBeyondEffect :: EffectArgs -> PushedIntoTheBeyondEffect
pushedIntoTheBeyondEffect = cardEffect PushedIntoTheBeyondEffect Cards.pushedIntoTheBeyond

instance RunMessage PushedIntoTheBeyondEffect where
  runMessage msg e@(PushedIntoTheBeyondEffect attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == toId attrs -> do
      push $ DiscardTopOfDeck iid 3 attrs.source (Just $ EffectTarget eid)
      pure e
    DiscardedTopOfDeck iid cards _ (EffectTarget eid) | eid == toId attrs -> do
      case attrs.metadata of
        Just (EffectCardCodes [x]) -> do
          pushWhen (x `elem` map (cdCardCode . toCardDef) cards) (assignHorror iid attrs.source 2)
        _ -> throwIO (InvalidState "Must have one card as the target")
      pure e
    _ -> PushedIntoTheBeyondEffect <$> runMessage msg attrs
