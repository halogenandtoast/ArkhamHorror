module Arkham.Investigator.Cards.JoeDiamond (joeDiamond) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Events
import Arkham.Helpers
import Arkham.Helpers.Deck
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Deck
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Phase
import Arkham.Placement
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Insight))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window
import Data.Map.Strict qualified as Map

newtype Metadata = Metadata {revealedHunchCard :: Maybe CardId}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype JoeDiamond = JoeDiamond (InvestigatorAttrs `With` Metadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance IsInvestigator JoeDiamond where
  investigatorFromAttrs = JoeDiamond . (`with` Metadata Nothing)

joeDiamond :: InvestigatorCard JoeDiamond
joeDiamond =
  investigator (JoeDiamond . (`with` Metadata Nothing)) Cards.joeDiamond
    $ Stats {health = 8, sanity = 6, willpower = 2, intellect = 4, combat = 4, agility = 2}

hunchDeck :: InvestigatorAttrs -> [Card]
hunchDeck = Map.findWithDefault [] HunchDeck . investigatorDecks

instance HasModifiersFor JoeDiamond where
  getModifiersFor (JoeDiamond (a `With` Metadata mcid)) =
    for_ mcid \cid -> case hunchDeck a of
      x : _ | x.id == cid -> modifySelf a [ReduceCostOf (CardWithId x.id) 2, AsIfInHandForPlay x.id]
      _ -> pure mempty

instance HasAbilities JoeDiamond where
  getAbilities (JoeDiamond (a `With` _)) =
    selfAbility_ a 1 (forced $ PhaseBegins #when #investigation)
      : case hunchDeck a of
        c : _
          | c.cardCode == Events.unsolvedCase.cardCode ->
              [ mkAbility a 2
                  $ SilentForcedAbility
                  $ WouldBeShuffledIntoDeck (DeckIs $ Deck.HunchDeck a.id) (cardIs Events.unsolvedCase)
              ]
        _ -> []

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
          hunchDeck' <- shuffleM $ map toCard insights
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
          focusCards (map toCard remainingInsights) do
            push $ ShuffleCardsIntoDeck (Deck.HunchDeck iid) [toCard unsolvedCase]
            questionLabel "Choose 10 more cards for hunch deck" iid
              $ ChooseN 10
              $ [ targetLabel insight.id [ShuffleCardsIntoDeck (Deck.HunchDeck iid) [toCard insight]]
                | insight <- remainingInsights
                ]
          pure $ JoeDiamond (attrs' `with` meta)
    ShuffleCardsIntoDeck (Deck.HunchDeck iid) [insight] | attrs `is` iid -> do
      hunchDeck' <- shuffleM $ insight : filter (/= insight) (hunchDeck attrs)
      pure
        $ JoeDiamond
        . (`with` Metadata Nothing)
        $ attrs
        & (deckL %~ filter ((/= insight) . toCard))
        & (decksL . at HunchDeck ?~ hunchDeck')
    Do (CheckWindows [Window Timing.When (Window.PhaseEnds InvestigationPhase) _]) -> do
      case hunchDeck attrs of
        x : _ | Just x.id == revealedHunchCard meta -> do
          checkWhen $ Window.WouldBeShuffledIntoDeck (Deck.HunchDeck attrs.id) x
          push $ ShuffleCardsIntoDeck (Deck.HunchDeck attrs.id) [x]
        _ -> pure ()
      JoeDiamond . (`with` meta) <$> liftRunMessage msg attrs
    InitiatePlayCard iid card _ _ _ _ | attrs `is` iid && Just card.id == revealedHunchCard meta -> do
      costModifier iid iid (ReduceCostOf (CardWithId card.id) 2)
      let hunchDeck' = filter (/= card) (hunchDeck attrs)
      attrs' <- liftRunMessage msg (attrs & decksL . at HunchDeck ?~ hunchDeck')
      pure $ JoeDiamond $ attrs' `with` Metadata Nothing
    CreateEventAt _ card _ -> do
      let hunchDeck' = filter (/= card) (hunchDeck attrs)
      pure $ JoeDiamond . (`with` Metadata Nothing) $ attrs & decksL . at HunchDeck ?~ hunchDeck'
    ResolveChaosToken _drawnToken ElderSign iid | attrs `is` iid -> do
      insights <- filter (`cardMatch` (CardWithTrait Insight <> #event)) <$> field InvestigatorDiscard iid
      when (notNull insights) do
        chooseOneM iid do
          labeled "Do not move an insight" nothing
          targets insights $ putCardOnBottomOfDeck iid (Deck.HunchDeck iid)
      pure i
    PutCardOnBottomOfDeck _ (Deck.HunchDeck iid) insight | attrs `is` iid -> do
      attrs' <- liftRunMessage msg attrs
      let hunchDeck' = filter (/= insight) (hunchDeck attrs) <> [insight]
      pure $ JoeDiamond . (`with` meta) $ attrs' & decksL . at HunchDeck ?~ hunchDeck'
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      case hunchDeck attrs of
        c : _ | c.cardCode == Events.unsolvedCase.cardCode -> do
          let
            go = \case
              ((windowType -> Window.WouldBeShuffledIntoDeck _ card) : _) -> do
                don'tMatching \case
                  ShuffleCardsIntoDeck (Deck.HunchDeck _) [card'] -> card == card'
                  _ -> False
                chooseOneM attrs.id do
                  abilityLabeled
                    attrs.id
                    (mkAbility (SourceableWithCardCode card $ CardIdSource card.id) 1 $ forced AnyWindow)
                    do
                      push $ CreateEventAt attrs.id card (InThreatArea attrs.id)
              (_ : xs) -> go xs
              [] -> pure ()
          go ws
        _ -> pure ()
      pure i
    _ -> JoeDiamond . (`with` meta) <$> liftRunMessage msg attrs
