module Arkham.Investigator.Cards.JennyBarnesParallel (jennyBarnesParallel, JennyBarnesParallel (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.Game.Helpers (getIsPlayableWithResources, getSpendableResources)
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (AssetCard)
import Arkham.Window (duringTurnWindow)

data Meta = Meta {responseCard :: Maybe Card, resourcesGained :: Int}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype JennyBarnesParallel = JennyBarnesParallel (InvestigatorAttrs `With` Meta)
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

jennyBarnesParallel :: InvestigatorCard JennyBarnesParallel
jennyBarnesParallel =
  investigator (JennyBarnesParallel . (`with` Meta Nothing 0)) Cards.jennyBarnesParallel
    $ Stats {health = 8, sanity = 7, willpower = 3, intellect = 3, combat = 3, agility = 3}

instance HasModifiersFor JennyBarnesParallel where
  getModifiersFor (JennyBarnesParallel (attrs `With` meta)) =
    case responseCard meta of
      Nothing -> pure mempty
      Just card -> modified_ attrs card [ReduceCostOf (CardWithId card.id) 1]

instance HasAbilities JennyBarnesParallel where
  getAbilities (JennyBarnesParallel (With a meta)) =
    [ restrictedAbility
        a
        1
        (Self <> PlayableCardExistsWithCostReduction (Reduce 1) (InHandOf You <> #talent <> #asset))
        $ freeReaction (TurnBegins #after You)
    , restrictedAbility a 2 (Self <> can.gain.resources You <> criteria)
        $ FastAbility
        $ DiscardAssetCost
        $ assetControlledBy a.id
        <> #talent
        <> AssetCardMatch CardWithNonZeroCost
    ]
   where
    criteria = if resourcesGained meta >= 5 then Never else NoRestriction

instance HasChaosTokenValue JennyBarnesParallel where
  getChaosTokenValue iid ElderSign (JennyBarnesParallel attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage JennyBarnesParallel where
  runMessage msg i@(JennyBarnesParallel (With attrs meta)) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' payment -> do
      results <- select (InHandOf (InvestigatorWithId iid) <> #talent <> #asset)
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
      let choose' c = UseCardAbilityChoiceTarget iid (toSource attrs) 1 (toTarget c) windows' payment
      chooseOne iid [targetLabel (toCardId c) [choose' c] | c <- cards]
      pure i
    UseCardAbilityChoiceTarget iid (isSource attrs -> True) 1 (CardIdTarget cid) _ _ -> do
      card <- getCard cid
      pushAll [PayCardCost iid card [duringTurnWindow iid], ResetMetadata (toTarget attrs)]
      pure . JennyBarnesParallel $ attrs `with` Meta (Just card) (resourcesGained meta)
    ResetMetadata (isTarget attrs -> True) ->
      pure . JennyBarnesParallel $ attrs `with` Meta Nothing (resourcesGained meta)
    UseCardAbility iid (isSource attrs -> True) 2 _ (discardPayments -> [(_, card)]) -> do
      let n = min (5 - resourcesGained meta) $ printedCardCost card
      gainResourcesIfCan iid (attrs.ability 2) n
      pure . JennyBarnesParallel $ attrs `with` Meta (responseCard meta) (n + resourcesGained meta)
    EndRound -> do
      pure . JennyBarnesParallel $ attrs `with` Meta Nothing 0
    PassedSkillTestWithToken iid ElderSign | attrs `is` iid -> do
      gainResourcesIfCan iid attrs 3
      pure i
    ResetGame -> do
      attrs' <- liftRunMessage msg attrs
      pure $ JennyBarnesParallel $ attrs' `with` Meta Nothing 0
    _ -> JennyBarnesParallel . (`with` meta) <$> liftRunMessage msg attrs
