module Arkham.Types.Investigator.Cards.NormanWithers where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Matcher hiding (PlayCard)
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype Metadata = Metadata { playedFromTopOfDeck :: Bool }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

newtype NormanWithers = NormanWithers (InvestigatorAttrs `With` Metadata)
  deriving anyclass IsInvestigator
  deriving newtype (Show, ToJSON, FromJSON, Entity)

normanWithers :: NormanWithers
normanWithers = NormanWithers . (`with` Metadata False) $ baseAttrs
  "08004"
  ("Norman Withers" <:> "The Astronomer")
  Seeker
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 5
    , combat = 2
    , agility = 1
    }
  [Miskatonic]

instance HasModifiersFor env NormanWithers where
  getModifiersFor _ target (NormanWithers (a `With` metadata))
    | isTarget a target
    = pure
      $ toModifiers a
      $ TopCardOfDeckIsRevealed
      : [ CanPlayTopOfDeck AnyCard | not (playedFromTopOfDeck metadata) ]
  getModifiersFor _ (CardIdTarget cid) (NormanWithers (a `With` _)) =
    case unDeck (investigatorDeck a) of
      x : _ | toCardId x == cid ->
        pure $ toModifiers a [ReduceCostOf (CardWithId cid) 1]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities NormanWithers where
  getAbilities (NormanWithers (a `With` _)) =
    [ restrictedAbility
          a
          1
          (Self
          <> InvestigatorExists (TopCardOfDeckIs WeaknessCard)
          <> CanManipulateDeck
          )
        $ ForcedAbility AnyWindow
    ]

instance HasTokenValue env NormanWithers where
  getTokenValue (NormanWithers (a `With` _)) iid ElderSign | iid == toId a = do
    let
      x = case unDeck (investigatorDeck a) of
        [] -> 0
        c : _ -> maybe 0 toPrintedCost (cdCost $ toCardDef c)
    pure $ TokenValue ElderSign (PositiveModifier x)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance InvestigatorRunner env => RunMessage env NormanWithers where
  runMessage msg nw@(NormanWithers (a `With` metadata)) = case msg of
    UseCardAbility iid source _ 1 _ | isSource a source ->
      nw <$ push (DrawCards iid 1 False)
    When (RevealToken _ iid token)
      | iid == toId a && tokenFace token == ElderSign -> do
        nw <$ push
          (chooseOne iid
          $ Label "Do not swap" []
          : [ TargetLabel
                (CardIdTarget $ toCardId c)
                [DrawCards iid 1 False, PutOnTopOfDeck iid c]
            | c <- mapMaybe (preview _PlayerCard) (investigatorHand a)
            ]
          )
    BeginRound -> NormanWithers . (`with` Metadata False) <$> runMessage msg a
    PlayCard iid cardId _ False | iid == toId a ->
      case unDeck (investigatorDeck a) of
        c : _ | toCardId c == cardId ->
          NormanWithers . (`with` Metadata True) <$> runMessage msg a
        _ -> NormanWithers . (`with` metadata) <$> runMessage msg a
    _ -> NormanWithers . (`with` metadata) <$> runMessage msg a
