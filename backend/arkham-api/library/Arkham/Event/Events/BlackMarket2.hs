module Arkham.Event.Events.BlackMarket2 (blackMarket2, blackMarket2Effect) where

import Arkham.Capability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getCard, getPhase)
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_, modified_)
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified
import Arkham.Projection
import Arkham.Window qualified as Window

newtype BlackMarket2 = BlackMarket2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackMarket2 :: EventCard BlackMarket2
blackMarket2 = event BlackMarket2 Cards.blackMarket2

instance RunMessage BlackMarket2 where
  runMessage msg e@(BlackMarket2 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid eid | eid == attrs.id -> do
      push $ DoStep 5 msg
      pure . BlackMarket2 $ attrs & setMeta @[CardId] []
    DoStep n msg'@(PlayThisEvent iid eid) | eid == attrs.id && n > 0 -> do
      investigators <- select $ affectsOthers can.manipulate.deck
      if null investigators
        then push $ DoStep 0 msg'
        else do
          chooseOrRunOne iid $ targetLabels investigators $ only . handleTargetChoice iid attrs
          push $ DoStep (n - 1) msg'
      pure e
    DoStep 0 (PlayThisEvent _iid eid) | eid == attrs.id -> do
      for_ (toResult @[CardId] attrs.meta) $ createCardEffect Cards.blackMarket2 Nothing attrs
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (InvestigatorTarget iid') -> do
      cards <- fieldMap InvestigatorDeck (map toCard . take 1 . unDeck) iid'
      pushAll $ map (ObtainCard . toCardId) cards <> [SetAsideCards cards]
      pure . BlackMarket2 $ attrs & overMeta (map toCardId cards <>)
    _ -> BlackMarket2 <$> liftRunMessage msg attrs

newtype BlackMarket2Effect = BlackMarket2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackMarket2Effect :: EffectArgs -> BlackMarket2Effect
blackMarket2Effect = cardEffectWith BlackMarket2Effect Cards.blackMarket2 (setEffectMeta False)

instance HasModifiersFor BlackMarket2Effect where
  getModifiersFor (BlackMarket2Effect attrs) = do
    case attrs.target of
      CardIdTarget cardId -> do
        investigators <- allInvestigators
        selectOne (SetAsideCardMatch $ CardWithId cardId) >>= \case
          Just card -> case card.owner of
            Just iid -> do
              for_ investigators \iid' ->
                if iid == iid'
                  then modified_ attrs iid' [AsIfInHand card]
                  else modifiedWhen_ attrs (not (isSignature card)) iid' [AsIfInHand card]
            Nothing -> for_ investigators \iid' ->
              modifiedWhen_ attrs (not (isSignature card)) iid' [AsIfInHand card]
          _ -> pure mempty -- should be disabled
      _ -> error "incorrect target"

instance RunMessage BlackMarket2Effect where
  runMessage msg e@(BlackMarket2Effect attrs) = runQueueT $ case msg of
    EndPhase -> do
      phase <- getPhase
      when (phase == #investigation) do
        case attrs.target of
          CardIdTarget cardId -> do
            card <- getCard cardId
            case card.owner of
              Nothing -> do
                investigators <- select Anyone
                lead <- getLead
                focusCard card do
                  chooseOrRunOneM lead do
                    questionLabeled "A set aside card was missing its owner. Please select the correct owner"
                    targets investigators \iid -> do
                      push $ ForTarget (toTarget attrs) (ForInvestigator iid (ForTarget (toTarget cardId) EndPhase))
              Just owner -> do
                obtainCard card
                push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck owner) [card]
          _ -> error "incorrect target"
        disable attrs
      pure e
    ForTarget (isTarget attrs -> True) (ForInvestigator iid (ForTarget (CardIdTarget cardId) EndPhase)) -> do
      card <- getCard cardId
      eliminated <- iid <!=~> UneliminatedInvestigator
      if eliminated
        then obtainCard card
        else do
          card' <- setOwner iid card
          push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card']
      pure e
    InitiatePlayCard iid card mtarget payment windows' asAction | attrs.target == CardIdTarget card.id -> do
      if cdSkipPlayWindows (toCardDef card)
        then push $ Arkham.Message.PlayCard iid card mtarget payment windows' asAction
        else do
          checkWindows [Window.mkWhen (Window.PlayCard iid $ Window.CardPlay card asAction)]
          push $ Arkham.Message.PlayCard iid card mtarget payment windows' asAction
          checkWindows [Window.mkAfter (Window.PlayCard iid $ Window.CardPlay card asAction)]
          push $ ResolvedPlayCard iid card
      pure e
    CardEnteredPlay _ card | attrs.target == CardIdTarget card.id -> do
      disableReturn e
    _ -> BlackMarket2Effect <$> liftRunMessage msg attrs
