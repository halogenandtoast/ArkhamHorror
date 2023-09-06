module Arkham.Investigator.Cards.LeoAnderson (
  leoAnderson,
  LeoAnderson (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (PlayCard)
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype Metadata = Metadata {responseCard :: Maybe Card}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LeoAnderson = LeoAnderson (InvestigatorAttrs `With` Metadata)
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leoAnderson :: InvestigatorCard LeoAnderson
leoAnderson =
  investigator
    (LeoAnderson . (`with` Metadata Nothing))
    Cards.leoAnderson
    Stats
      { health = 8
      , sanity = 6
      , willpower = 4
      , intellect = 3
      , combat = 4
      , agility = 1
      }

instance HasModifiersFor LeoAnderson where
  getModifiersFor (CardIdTarget cid) (LeoAnderson (attrs `With` metadata))
    | Just cid == fmap toCardId (responseCard metadata) =
        pure $
          toModifiers attrs [ReduceCostOf (CardWithId cid) 1]
  getModifiersFor _ _ = pure []

instance HasAbilities LeoAnderson where
  getAbilities (LeoAnderson a) =
    [ restrictedAbility
        a
        1
        ( Self
            <> PlayableCardExistsWithCostReduction
              1
              (InHandOf You <> BasicCardMatch IsAlly)
        )
        $ ReactionAbility (TurnBegins Timing.After You) Free
    ]

instance HasChaosTokenValue LeoAnderson where
  getChaosTokenValue iid ElderSign (LeoAnderson attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage LeoAnderson where
  runMessage msg i@(LeoAnderson (attrs `With` metadata)) = case msg of
    UseCardAbility iid source 1 windows' payment | isSource attrs source -> do
      results <- selectList (InHandOf You <> BasicCardMatch IsAlly)
      spendableResources <- getSpendableResources iid
      cards <-
        filterM
          ( getIsPlayableWithResources
              iid
              source
              (spendableResources + 1)
              UnpaidCost
              [mkWindow Timing.When (Window.DuringTurn iid)]
          )
          results
      push $
        chooseOne
          iid
          [ TargetLabel
            (CardIdTarget $ toCardId c)
            [ UseCardAbilityChoiceTarget
                iid
                source
                1
                (CardTarget c)
                windows'
                payment
            ]
          | c <- cards
          ]
      pure i
    UseCardAbilityChoiceTarget iid source 1 (CardTarget card) _ _
      | isSource attrs source -> do
          let windows' = [mkWindow Timing.When (Window.DuringTurn iid)]
          pushAll [PayCardCost iid card windows', ResetMetadata (toTarget attrs)]
          pure . LeoAnderson $ attrs `with` Metadata (Just card)
    ResetMetadata (isTarget attrs -> True) ->
      pure . LeoAnderson $ attrs `with` Metadata Nothing
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      push $
        Search
          iid
          (toSource attrs)
          (toTarget attrs)
          [(FromTopOfDeck 3, ShuffleBackIn)]
          IsAlly
          (DrawFound iid 1)
      pure i
    _ -> LeoAnderson . (`with` metadata) <$> runMessage msg attrs
