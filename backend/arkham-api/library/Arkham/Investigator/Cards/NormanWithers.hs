module Arkham.Investigator.Cards.NormanWithers (normanWithers) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers
import Arkham.Helpers.ChaosToken
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (PlayCard, RevealChaosToken)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Treacheries

data Metadata = Metadata
  { playedFromTopOfDeck :: Bool
  , drawingForcedWeakness :: Bool
  }
  deriving stock (Show, Generic, Eq, Data)
  deriving anyclass (ToJSON, FromJSON)

defaultMetadata :: Metadata
defaultMetadata = Metadata {playedFromTopOfDeck = False, drawingForcedWeakness = False}

getMetadata :: InvestigatorAttrs -> Metadata
getMetadata a = toResultDefault defaultMetadata a.meta

newtype NormanWithers = NormanWithers InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

normanWithers :: InvestigatorCard NormanWithers
normanWithers =
  investigator NormanWithers Cards.normanWithers
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 5, combat = 2, agility = 1}

instance HasModifiersFor NormanWithers where
  getModifiersFor (NormanWithers a) = do
    let metadata = getMetadata a
    canReveal <- withoutModifier a CannotRevealCards
    modifySelfWhen a canReveal
      $ TopCardOfDeckIsRevealed
      : [CanPlayTopOfDeck AnyCard | not (playedFromTopOfDeck metadata)]
    case unDeck (investigatorDeck a) of
      x : _ -> modifiedWhen_ a canReveal x [ReduceCostOf (CardWithId x.id) 1]
      _ -> pure ()

instance HasAbilities NormanWithers where
  getAbilities (NormanWithers a) =
    [ selfAbility
        a
        1
        ( youExist
            ( TopCardOfDeckIs (WeaknessCard <> not_ (cardIs Treacheries.theHarbinger))
                <> not_ (InvestigatorWithMetaKey "drawingForcedWeakness")
            )
            <> CanManipulateDeck
            <> NotSetup
        )
        (forced AnyWindow)
    ]

instance HasChaosTokenValue NormanWithers where
  getChaosTokenValue iid ElderSign (NormanWithers a) | iid == toId a = do
    let
      x = case unDeck (investigatorDeck a) of
        [] -> 0
        c : _ -> maybe 0 toPrintedCost (cdCost $ toCardDef c)
    pure $ ChaosTokenValue ElderSign (PositiveModifier x)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage NormanWithers where
  runMessage msg i@(NormanWithers a) = case msg of
    UseThisAbility iid (isSource a -> True) 1 -> do
      push $ drawCards iid (a.ability 1) 1
      let metadata = getMetadata a
      pure $ NormanWithers $ a & setMeta (metadata {drawingForcedWeakness = True})
    When (RevealChaosToken _ iid token) | iid == toId a -> do
      faces <- getModifiedChaosTokenFace token
      when (ElderSign `elem` faces) $ do
        hand <- field InvestigatorHand iid
        player <- getPlayer iid
        push
          $ chooseOne player
          $ Label "$label.doNotSwap" []
          : [ targetLabel
                (toCardId c)
                [ drawCards iid (ChaosTokenEffectSource ElderSign) 1
                , PutCardOnTopOfDeck iid (Deck.InvestigatorDeck iid) (toCard c)
                ]
            | c <- onlyPlayerCards hand
            ]
      pure i
    Do BeginRound -> do
      attrs' <- runMessage msg a
      pure $ NormanWithers $ attrs' & setMeta defaultMetadata
    Do (DrawCards iid' _) | iid' == toId a -> do
      attrs' <- runMessage msg a
      let metadata = getMetadata attrs'
      pure $ NormanWithers $ attrs' & setMeta (metadata {drawingForcedWeakness = False})
    PlayCard iid card _ _ _ False | iid == toId a ->
      case unDeck (investigatorDeck a) of
        c : _ | toCardId c == toCardId card -> do
          attrs' <- runMessage msg a
          let metadata = getMetadata attrs'
          pure $ NormanWithers $ attrs' & setMeta (metadata {playedFromTopOfDeck = True})
        _ -> NormanWithers <$> runMessage msg a
    _ -> NormanWithers <$> runMessage msg a
