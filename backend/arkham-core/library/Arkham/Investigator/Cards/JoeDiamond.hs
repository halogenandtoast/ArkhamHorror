module Arkham.Investigator.Cards.JoeDiamond (joeDiamond, JoeDiamond (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers
import Arkham.Helpers.Deck
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Deck
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Phase
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Insight))
import Arkham.Window (Window (..), mkWindow)
import Arkham.Window qualified as Window
import Data.Map.Strict qualified as Map

newtype Metadata = Metadata {revealedHunchCard :: Maybe CardId}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype JoeDiamond = JoeDiamond (InvestigatorAttrs `With` Metadata)
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

joeDiamond :: InvestigatorCard JoeDiamond
joeDiamond =
  investigator (JoeDiamond . (`with` Metadata Nothing)) Cards.joeDiamond
    $ Stats {health = 8, sanity = 6, willpower = 2, intellect = 4, combat = 4, agility = 2}

hunchDeck :: InvestigatorAttrs -> [Card]
hunchDeck = Map.findWithDefault [] HunchDeck . investigatorDecks

instance HasModifiersFor JoeDiamond where
  getModifiersFor target (JoeDiamond (a `With` Metadata (Just cid))) | a `is` target = do
    case hunchDeck a of
      x : _ | x.id == cid -> modified a [ReduceCostOf (CardWithId $ toCardId x) 2, AsIfInHand x]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities JoeDiamond where
  getAbilities (JoeDiamond (a `With` _)) =
    [restrictedAbility a 1 Self $ forced $ PhaseBegins #when #investigation]

instance HasChaosTokenValue JoeDiamond where
  getChaosTokenValue iid ElderSign (JoeDiamond (attrs `With` _)) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JoeDiamond where
  runMessage msg i@(JoeDiamond (attrs `With` meta)) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      case hunchDeck attrs of
        x : _ -> pure . JoeDiamond $ attrs `with` meta {revealedHunchCard = Just x.id}
        _ -> pure i
    SetupInvestigator iid | attrs `is` iid -> do
      attrs' <- liftRunMessage msg attrs
      let insights = filter (`cardMatch` (CardWithTrait Insight <> #event)) (unDeck attrs.deck)
      if length insights == 11
        then do
          hunchDeck' <- shuffleM (map toCard insights)
          pure
            $ JoeDiamond
            . (`with` Metadata (revealedHunchCard meta))
            $ attrs'
            & (deckL %~ withDeck (filter (`notElem` insights)))
            & (decksL . at HunchDeck ?~ hunchDeck')
        else do
          let
            unsolvedCase =
              fromJustNote "Deck missing unsolved case"
                $ find (`cardMatch` cardIs Events.unsolvedCase) insights
            remainingInsights = filter (/= unsolvedCase) insights
          focusCards (map toCard remainingInsights) \unfocus -> do
            push $ ShuffleCardsIntoDeck (Deck.HunchDeck iid) [toCard unsolvedCase]
            questionLabel "Choose 10 more cards for hunch deck" iid
              $ ChooseN 10
              $ [ targetLabel
                  (toCardId insight)
                  [ShuffleCardsIntoDeck (Deck.HunchDeck iid) [toCard insight]]
                | insight <- remainingInsights
                ]
            push unfocus
          pure $ JoeDiamond (attrs' `with` meta)
    ShuffleCardsIntoDeck (Deck.HunchDeck iid) [insight] | attrs `is` iid -> do
      hunchDeck' <- shuffleM $ insight : filter (/= insight) (hunchDeck attrs)
      pure
        $ JoeDiamond
        . (`with` Metadata Nothing)
        $ attrs
        & (deckL %~ filter ((/= insight) . toCard))
        & (decksL . at HunchDeck ?~ hunchDeck')
    RunWindow iid [Window Timing.When (Window.PhaseEnds InvestigationPhase) _] | attrs `is` iid -> do
      case hunchDeck attrs of
        x : _ | Just x.id == revealedHunchCard meta -> do
          checkWindows [mkWindow #when (Window.WouldBeShuffledIntoDeck (Deck.HunchDeck iid) x)]
          push $ ShuffleCardsIntoDeck (Deck.HunchDeck iid) [x]
        _ -> pure ()
      pure i
    InitiatePlayCard iid card mTarget payment windows' asAction | attrs `is` iid && Just card.id == revealedHunchCard meta -> do
      addToHand iid [card]
      costModifier iid iid (ReduceCostOf (CardWithId $ toCardId card) 2)
      push $ InitiatePlayCard iid card mTarget payment windows' asAction
      let hunchDeck' = filter (/= card) (hunchDeck attrs)
      pure $ JoeDiamond . (`with` Metadata Nothing) $ attrs & decksL . at HunchDeck ?~ hunchDeck'
    CreateEventAt _ card _ -> do
      let hunchDeck' = filter (/= card) (hunchDeck attrs)
      pure $ JoeDiamond . (`with` Metadata Nothing) $ attrs & decksL . at HunchDeck ?~ hunchDeck'
    ResolveChaosToken _drawnToken ElderSign iid | attrs `is` iid -> do
      insights <- filter (`cardMatch` (CardWithTrait Insight <> #event)) <$> field InvestigatorDiscard iid
      when (notNull insights) do
        chooseOne iid
          $ Label "Do not move an insight" []
          : [ targetLabel
              (toCardId insight)
              [PutCardOnBottomOfDeck iid (Deck.HunchDeck iid) $ PlayerCard insight]
            | insight <- insights
            ]
      pure i
    PutCardOnBottomOfDeck _ (Deck.HunchDeck iid) insight | attrs `is` iid -> do
      attrs' <- liftRunMessage msg attrs
      let hunchDeck' = filter (/= insight) (hunchDeck attrs) <> [insight]
      pure $ JoeDiamond . (`with` meta) $ attrs' & decksL . at HunchDeck ?~ hunchDeck'
    _ -> JoeDiamond . (`with` meta) <$> liftRunMessage msg attrs
