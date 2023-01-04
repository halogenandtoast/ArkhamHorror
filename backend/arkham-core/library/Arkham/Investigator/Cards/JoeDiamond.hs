module Arkham.Investigator.Cards.JoeDiamond
  ( joeDiamond
  , JoeDiamond(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Card.Id
import Arkham.Criteria
import Arkham.Deck ( DeckSignifier (HunchDeck) )
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
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

data Metadata = Metadata
  { hunchDeck :: [PlayerCard]
  , revealedHunchCard :: Maybe CardId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype JoeDiamond = JoeDiamond (InvestigatorAttrs `With` Metadata)
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

joeDiamond :: InvestigatorCard JoeDiamond
joeDiamond = investigator
  (JoeDiamond . (`with` Metadata [] Nothing))
  Cards.joeDiamond
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 4
    , combat = 4
    , agility = 2
    }

instance HasModifiersFor JoeDiamond where
  getModifiersFor (CardIdTarget cid) (JoeDiamond (a `With` Metadata hunchDeck _))
    = case hunchDeck of
      x : _ | toCardId x == cid ->
        pure $ toModifiers a [ReduceCostOf (CardWithId cid) 2]
      _ -> pure []
  getModifiersFor target (JoeDiamond (a `With` Metadata hunchDeck (Just cid)))
    | isTarget a target = case hunchDeck of
      x : _ | toCardId x == cid ->
        pure $ toModifiers a [AsIfInHand $ PlayerCard x]
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
      case hunchDeck meta of
        x : _ -> pure . JoeDiamond $ attrs `with` meta
          { revealedHunchCard = Just (toCardId x)
          }
        _ -> pure i
    SetupInvestigator iid | iid == toId attrs -> do
      let
        insights = filter
          (`cardMatch` (CardWithTrait Insight <> CardWithType EventType))
          (unDeck $ investigatorDeck attrs)
      if length insights == 11
        then do
          hunchDeck <- shuffleM insights
          pure
            $ JoeDiamond
            . (`with` Metadata hunchDeck (revealedHunchCard meta))
            $ attrs
            & deckL
            %~ withDeck (filter (`notElem` insights))
        else do
          let
            unsolvedCase = fromJustNote "Deck missing unsolved case"
              $ find (`cardMatch` (cardIs Events.unsolvedCase)) insights
          pushAll
            [ FocusCards $ map PlayerCard insights
            , ShuffleCardsIntoDeck HunchDeck [PlayerCard unsolvedCase]
            , Ask (toId attrs)
            $ QuestionLabel "Choose 11 cards for hunch deck"
            $ ChooseN
                10
                [ TargetLabel
                    (CardIdTarget $ toCardId insight)
                    [ShuffleCardsIntoDeck HunchDeck [PlayerCard insight]]
                | insight <- filter (/= unsolvedCase) insights
                ]
            , UnfocusCards
            ]
          pure i

    ShuffleCardsIntoDeck HunchDeck [PlayerCard insight] -> do
      hunchDeck <- shuffleM (insight : hunchDeck meta)
      pure
        $ JoeDiamond
        . (`with` Metadata hunchDeck (revealedHunchCard meta))
        $ attrs
        & deckL
        %~ withDeck (filter (/= insight))
    RunWindow iid [Window Timing.When (Window.PhaseEnds InvestigationPhase)]
      | iid == toId attrs -> do
        case hunchDeck meta of
          x : xs | Just (toCardId x) == revealedHunchCard meta -> do
            wouldBeWindow <- checkWindows
              [ Window
                  Timing.When
                  (Window.WouldBeShuffledIntoDeck HunchDeck (PlayerCard x))
              ]
            pushAll
              [wouldBeWindow, ShuffleCardsIntoDeck HunchDeck [PlayerCard x]]
            pure . JoeDiamond $ attrs `with` meta
              { hunchDeck = xs
              , revealedHunchCard = Nothing
              }
          _ -> pure . JoeDiamond $ attrs `with` meta
            { revealedHunchCard = Nothing
            }
    InitiatePlayCard iid (PlayerCard card) mTarget windows' _
      | iid == toId attrs && Just (toCardId card) == revealedHunchCard meta -> do
        pushAll
          [ AddToHand iid $ PlayerCard card
          , InitiatePlayCard iid (PlayerCard card) mTarget windows' False
          ]
        pure
          $ JoeDiamond
          $ attrs
          `with` Metadata (filter (/= card) (hunchDeck meta)) Nothing
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
                [PutCardOnBottomOfDeck iid HunchDeck $ PlayerCard insight]
            | insight <- insights
            ]
      pure i
    PutCardOnBottomOfDeck iid HunchDeck (PlayerCard insight)
      | iid == toId attrs -> do
        attrs' <- runMessage msg attrs
        pure $ JoeDiamond $ attrs' `with` Metadata
          (hunchDeck meta <> [insight])
          (revealedHunchCard meta)
    _ -> JoeDiamond . (`with` meta) <$> runMessage msg attrs
