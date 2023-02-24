module Arkham.Investigator.Cards.JoeDiamond
  ( joeDiamond
  , JoeDiamond(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Card.Id
import Arkham.Criteria
import Arkham.Deck qualified as Deck
import Arkham.Investigator.Deck
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers
import Arkham.Helpers.Deck
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Data.HashMap.Strict qualified as HashMap

data Metadata = Metadata
  { revealedHunchCard :: Maybe CardId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype JoeDiamond = JoeDiamond (InvestigatorAttrs `With` Metadata)
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeDiamond :: InvestigatorCard JoeDiamond
joeDiamond = investigator
  (JoeDiamond . (`with` Metadata Nothing))
  Cards.joeDiamond
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 4
    , combat = 4
    , agility = 2
    }

hunchDeck :: InvestigatorAttrs -> [Card]
hunchDeck = HashMap.findWithDefault [] HunchDeck . investigatorDecks

instance HasModifiersFor JoeDiamond where
  getModifiersFor (CardIdTarget cid) (JoeDiamond (a `With` Metadata _))
    = case hunchDeck a of
      x : _ | toCardId x == cid ->
        pure $ toModifiers a [ReduceCostOf (CardWithId cid) 2]
      _ -> pure []
  getModifiersFor target (JoeDiamond (a `With` Metadata (Just cid)))
    | isTarget a target = case hunchDeck a of
      x : _ | toCardId x == cid ->
        pure $ toModifiers a [AsIfInHand x]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities JoeDiamond where
  getAbilities (JoeDiamond (a `With` _)) =
    [ restrictedAbility a 1 Self
        $ ForcedAbility
        $ PhaseBegins Timing.When
        $ PhaseIs InvestigationPhase
    ]

instance HasTokenValue JoeDiamond where
  getTokenValue iid ElderSign (JoeDiamond (attrs `With` _))
    | iid == toId attrs = do
      pure $ TokenValue ElderSign $ PositiveModifier 1
  getTokenValue _ token _ = pure $ TokenValue token mempty

instance RunMessage JoeDiamond where
  runMessage msg i@(JoeDiamond (attrs `With` meta)) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      case hunchDeck attrs of
        x : _ -> pure . JoeDiamond $ attrs `with` meta
          { revealedHunchCard = Just (toCardId x)
          }
        _ -> pure i
    SetupInvestigator iid | iid == toId attrs -> do
      attrs' <- runMessage msg attrs
      let
        insights = filter
          (`cardMatch` (CardWithTrait Insight <> CardWithType EventType))
          (unDeck $ investigatorDeck attrs)
      if length insights == 11
        then do
          hunchDeck' <- shuffleM (map PlayerCard insights)
          pure
            $ JoeDiamond
            . (`with` Metadata (revealedHunchCard meta))
            $ attrs'
            & (deckL %~ withDeck (filter (`notElem` insights)))
            & (decksL . at HunchDeck ?~ hunchDeck')
        else do
          let
            unsolvedCase = fromJustNote "Deck missing unsolved case"
              $ find (`cardMatch` (cardIs Events.unsolvedCase)) insights
            remainingInsights = filter (/= unsolvedCase) insights
          pushAll
            [ FocusCards $ map PlayerCard remainingInsights
            , ShuffleCardsIntoDeck (Deck.InvestigatorDeckByKey iid HunchDeck) [PlayerCard unsolvedCase]
            , Ask (toId attrs)
            $ QuestionLabel "Choose 10 more cards for hunch deck"
            $ ChooseN
                10
                [ TargetLabel
                    (CardIdTarget $ toCardId insight)
                    [ShuffleCardsIntoDeck (Deck.InvestigatorDeckByKey iid HunchDeck) [PlayerCard insight]]
                | insight <- remainingInsights
                ]
            , UnfocusCards
            ]
          pure $ JoeDiamond (attrs' `with` meta)
    ShuffleCardsIntoDeck (Deck.InvestigatorDeckByKey iid HunchDeck) [insight] | iid == toId attrs -> do
      hunchDeck' <- shuffleM (insight : hunchDeck attrs)
      pure
        $ JoeDiamond
        . (`with` Metadata Nothing)
        $ attrs
        & deckL %~ withDeck (filter ((/= insight) . PlayerCard))
        & decksL . at HunchDeck ?~ hunchDeck'
    RunWindow iid [Window Timing.When (Window.PhaseEnds InvestigationPhase)]
      | iid == toId attrs -> do
        case hunchDeck attrs of
          x : _ | Just (toCardId x) == revealedHunchCard meta -> do
            wouldBeWindow <- checkWindows
              [ Window
                  Timing.When
                  (Window.WouldBeShuffledIntoDeck (Deck.InvestigatorDeckByKey iid HunchDeck) x)
              ]
            pushAll
              [wouldBeWindow, ShuffleCardsIntoDeck (Deck.InvestigatorDeckByKey iid HunchDeck) [x]]
          _ -> pure ()
        pure i
    InitiatePlayCard iid card mTarget windows' _
      | iid == toId attrs && Just (toCardId card) == revealedHunchCard meta -> do
        pushAll
          [ AddToHand iid card
          , InitiatePlayCard iid card mTarget windows' False
          ]
        let hunchDeck' = filter (/= card) (hunchDeck attrs)
        pure
          $ JoeDiamond . (`with` Metadata Nothing)
          $ attrs & decksL . at HunchDeck ?~ hunchDeck'
    CreateEventAt _ card _ -> do
      let hunchDeck' = filter (/= card) (hunchDeck attrs)
      pure
        $ JoeDiamond . (`with` Metadata Nothing)
        $ attrs & decksL . at HunchDeck ?~ hunchDeck'
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs -> do
      insights <-
        filter (`cardMatch` (CardWithTrait Insight <> CardWithType EventType))
          <$> field InvestigatorDiscard iid
      unless (null insights) $ do
        push
          $ chooseOne iid
          $ Label "Do not move an insight" []
          : [ TargetLabel
                (CardIdTarget $ toCardId insight)
                [PutCardOnBottomOfDeck iid (Deck.InvestigatorDeckByKey iid HunchDeck) $ PlayerCard insight]
            | insight <- insights
            ]
      pure i
    PutCardOnBottomOfDeck _ (Deck.InvestigatorDeckByKey iid HunchDeck) insight
      | iid == toId attrs -> do
        attrs' <- runMessage msg attrs
        pure $ JoeDiamond . (`with` meta) $ attrs' & decksL . at HunchDeck ?~ hunchDeck attrs <> [insight]
    _ -> JoeDiamond . (`with` meta) <$> runMessage msg attrs
