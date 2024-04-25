module Arkham.Investigator.Cards.LeoAnderson (
  leoAnderson,
  LeoAnderson (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (PlayCard)
import Arkham.Window (duringTurnWindow)

newtype Meta = Meta {responseCard :: Maybe Card}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LeoAnderson = LeoAnderson (InvestigatorAttrs `With` Meta)
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leoAnderson :: InvestigatorCard LeoAnderson
leoAnderson =
  investigator (LeoAnderson . (`with` Meta Nothing)) Cards.leoAnderson
    $ Stats {health = 8, sanity = 6, willpower = 4, intellect = 3, combat = 4, agility = 1}

instance HasModifiersFor LeoAnderson where
  getModifiersFor (CardIdTarget cid) (LeoAnderson (attrs `With` meta))
    | cid `elem` fmap toCardId (responseCard meta) = do
        pure $ toModifiers attrs [ReduceCostOf (CardWithId cid) 1]
  getModifiersFor _ _ = pure []

instance HasAbilities LeoAnderson where
  getAbilities (LeoAnderson a) =
    [ restrictedAbility a 1 (Self <> PlayableCardExistsWithCostReduction 1 (InHandOf You <> #ally))
        $ freeReaction (TurnBegins #after You)
    ]

instance HasChaosTokenValue LeoAnderson where
  getChaosTokenValue iid ElderSign (LeoAnderson attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage LeoAnderson where
  runMessage msg i@(LeoAnderson (attrs `With` meta)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' payment -> do
      results <- select (InHandOf (InvestigatorWithId iid) <> #ally)
      resources <- getSpendableResources iid
      cards <-
        filterM
          ( getIsPlayableWithResources
              iid
              GameSource
              (resources + 1)
              (UnpaidCost NoAction)
              [duringTurnWindow iid]
          )
          results
      let choose c = UseCardAbilityChoiceTarget iid (toSource attrs) 1 (CardTarget c) windows' payment
      player <- getPlayer iid
      push $ chooseOne player [targetLabel (toCardId c) [choose c] | c <- cards]
      pure i
    UseCardAbilityChoiceTarget iid (isSource attrs -> True) 1 (CardTarget card) _ _ -> do
      pushAll [PayCardCost iid card [duringTurnWindow iid], ResetMetadata (toTarget attrs)]
      pure . LeoAnderson $ attrs `with` Meta (Just card)
    ResetMetadata (isTarget attrs -> True) ->
      pure . LeoAnderson $ attrs `with` Meta Nothing
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      push $ search iid attrs attrs [fromTopOfDeck 3] #ally (DrawFound iid 1)
      pure i
    _ -> LeoAnderson . (`with` meta) <$> runMessage msg attrs
