module Arkham.Investigator.Cards.NormanWithers where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Game.Helpers
import Arkham.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (PlayCard, RevealChaosToken)
import Arkham.Prelude
import Arkham.Projection

newtype Metadata = Metadata {playedFromTopOfDeck :: Bool}
  deriving stock (Show, Generic, Eq, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype NormanWithers = NormanWithers (InvestigatorAttrs `With` Metadata)
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

normanWithers :: InvestigatorCard NormanWithers
normanWithers =
  investigator (NormanWithers . (`with` Metadata False)) Cards.normanWithers
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 5, combat = 2, agility = 1}

instance HasModifiersFor NormanWithers where
  getModifiersFor target (NormanWithers (a `With` metadata)) | isTarget a target = do
    canReveal <- withoutModifier a CannotRevealCards
    pure
      $ toModifiers a
      $ guard canReveal
      *> ( TopCardOfDeckIsRevealed
            : [CanPlayTopOfDeck AnyCard | not (playedFromTopOfDeck metadata)]
         )
  getModifiersFor (CardIdTarget cid) (NormanWithers (a `With` _)) =
    case unDeck (investigatorDeck a) of
      x : _ | toCardId x == cid -> do
        canReveal <- withoutModifier a CannotRevealCards
        pure $ toModifiers a [ReduceCostOf (CardWithId cid) 1 | canReveal]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities NormanWithers where
  getAbilities (NormanWithers (a `With` _)) =
    [ restrictedAbility
        a
        1
        (Self <> exists (TopCardOfDeckIs WeaknessCard) <> CanManipulateDeck)
        (forced AnyWindow)
    ]

instance HasChaosTokenValue NormanWithers where
  getChaosTokenValue iid ElderSign (NormanWithers (a `With` _)) | iid == toId a = do
    let
      x = case unDeck (investigatorDeck a) of
        [] -> 0
        c : _ -> maybe 0 toPrintedCost (cdCost $ toCardDef c)
    pure $ ChaosTokenValue ElderSign (PositiveModifier x)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage NormanWithers where
  runMessage msg nw@(NormanWithers (a `With` metadata)) = case msg of
    UseThisAbility iid (isSource a -> True) 1 -> do
      push $ drawCards iid (a.ability 1) 1
      pure nw
    When (RevealChaosToken _ iid token) | iid == toId a -> do
      faces <- getModifiedChaosTokenFace token
      when (ElderSign `elem` faces) $ do
        hand <- field InvestigatorHand iid
        player <- getPlayer iid
        push
          $ chooseOne player
          $ Label "Do not swap" []
          : [ targetLabel
              (toCardId c)
              [ drawCards iid (ChaosTokenEffectSource ElderSign) 1
              , PutCardOnTopOfDeck iid (Deck.InvestigatorDeck iid) (toCard c)
              ]
            | c <- onlyPlayerCards hand
            ]
      pure nw
    Do BeginRound -> NormanWithers . (`with` Metadata False) <$> runMessage msg a
    PlayCard iid card _ _ _ False | iid == toId a ->
      case unDeck (investigatorDeck a) of
        c : _ | toCardId c == toCardId card -> do
          NormanWithers . (`with` Metadata True) <$> runMessage msg a
        _ -> NormanWithers . (`with` metadata) <$> runMessage msg a
    _ -> NormanWithers . (`with` metadata) <$> runMessage msg a
