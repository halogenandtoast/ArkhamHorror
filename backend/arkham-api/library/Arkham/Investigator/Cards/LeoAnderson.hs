module Arkham.Investigator.Cards.LeoAnderson (leoAnderson) where

import Arkham.Ability
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Cost
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Playable
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (PlayCard)
import Arkham.Message.Lifted.Choose
import Arkham.Strategy
import Arkham.Window (duringTurnWindow)

newtype Meta = Meta {responseCard :: Maybe Card}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype LeoAnderson = LeoAnderson (InvestigatorAttrs `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance IsInvestigator LeoAnderson where
  investigatorFromAttrs = LeoAnderson . (`with` Meta Nothing)

leoAnderson :: InvestigatorCard LeoAnderson
leoAnderson =
  investigator (LeoAnderson . (`with` Meta Nothing)) Cards.leoAnderson
    $ Stats {health = 8, sanity = 6, willpower = 4, intellect = 3, combat = 4, agility = 1}

instance HasModifiersFor LeoAnderson where
  getModifiersFor (LeoAnderson (attrs `With` meta)) =
    for_ (responseCard meta) \card ->
      modified_ attrs card [ReduceCostOf (CardWithId card.id) 1]

instance HasAbilities LeoAnderson where
  getAbilities (LeoAnderson a) =
    [ restricted
        a
        1
        (Self <> PlayableCardExistsWithCostReduction (Reduce 1) (InHandOf ForPlay You <> #ally))
        $ freeReaction (TurnBegins #after You)
    ]

instance HasChaosTokenValue LeoAnderson where
  getChaosTokenValue iid ElderSign (LeoAnderson attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage LeoAnderson where
  runMessage msg i@(LeoAnderson (attrs `With` meta)) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' payment -> do
      results <- select (InHandOf ForPlay (InvestigatorWithId iid) <> #ally)
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
      let runChoose c = push $ UseCardAbilityChoiceTarget iid (toSource attrs) 1 (toTarget c) windows' payment
      chooseTargetM iid cards runChoose
      pure i
    UseCardAbilityChoiceTarget iid (isSource attrs -> True) 1 (CardIdTarget cid) _ _ -> do
      card <- getCard cid
      playCardPayingCost iid card
      push $ ResetMetadata (toTarget attrs)
      pure . LeoAnderson $ attrs `with` Meta (Just card)
    ResetMetadata (isTarget attrs -> True) ->
      pure . LeoAnderson $ attrs `with` Meta Nothing
    ResetGame -> LeoAnderson . (`with` Meta Nothing) <$> liftRunMessage msg attrs
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      search iid attrs attrs [fromTopOfDeck 3] #ally (DrawFound iid 1)
      pure i
    _ -> LeoAnderson . (`with` meta) <$> liftRunMessage msg attrs
