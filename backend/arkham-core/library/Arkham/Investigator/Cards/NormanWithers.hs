module Arkham.Investigator.Cards.NormanWithers where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Card.Cost
import Arkham.Deck qualified as Deck
import Arkham.Game.Helpers
import Arkham.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (PlayCard, RevealChaosToken)

newtype Metadata = Metadata {playedFromTopOfDeck :: Bool}
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

newtype NormanWithers = NormanWithers (InvestigatorAttrs `With` Metadata)
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

normanWithers :: InvestigatorCard NormanWithers
normanWithers =
  investigator (NormanWithers . (`with` Metadata False)) Cards.normanWithers
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 5, combat = 2, agility = 1}

instance HasModifiersFor NormanWithers where
  getModifiersFor target (NormanWithers (a `With` metadata)) | isTarget a target = do
    pure
      $ toModifiers a
      $ TopCardOfDeckIsRevealed : [CanPlayTopOfDeck AnyCard | not (playedFromTopOfDeck metadata)]
  getModifiersFor (CardIdTarget cid) (NormanWithers (a `With` _)) =
    case unDeck (investigatorDeck a) of
      x : _ | toCardId x == cid -> do
        pure $ toModifiers a [ReduceCostOf (CardWithId cid) 1]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities NormanWithers where
  getAbilities (NormanWithers (a `With` _)) =
    [ (restrictedAbility a 1)
        (Self <> InvestigatorExists (TopCardOfDeckIs WeaknessCard) <> CanManipulateDeck)
        (ForcedAbility AnyWindow)
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
    UseCardAbility iid (isSource a -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource a 1) 1
      pure nw
    When (RevealChaosToken _ iid token) | iid == toId a -> do
      faces <- getModifiedChaosTokenFace token
      when (ElderSign `elem` faces) $ do
        drawing <- drawCards iid (ChaosTokenEffectSource ElderSign) 1
        push
          $ chooseOne iid
          $ Label "Do not swap" []
            : [ targetLabel (toCardId c)
                $ [drawing, PutCardOnTopOfDeck iid (Deck.InvestigatorDeck iid) (toCard c)]
              | c <- onlyPlayerCards (investigatorHand a)
              ]
      pure nw
    BeginRound -> NormanWithers . (`with` Metadata False) <$> runMessage msg a
    PlayCard iid card _ _ False | iid == toId a ->
      case unDeck (investigatorDeck a) of
        c : _ | toCardId c == toCardId card -> do
          NormanWithers . (`with` Metadata True) <$> runMessage msg a
        _ -> NormanWithers . (`with` metadata) <$> runMessage msg a
    _ -> NormanWithers . (`with` metadata) <$> runMessage msg a
