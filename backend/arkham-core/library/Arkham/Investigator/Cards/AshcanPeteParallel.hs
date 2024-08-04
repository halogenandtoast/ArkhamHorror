module Arkham.Investigator.Cards.AshcanPeteParallel (
  ashcanPeteParallel,
  AshcanPeteParallel (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Window

newtype AshcanPeteParallel = AshcanPeteParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

ashcanPeteParallel :: InvestigatorCard AshcanPeteParallel
ashcanPeteParallel =
  startsWith [Assets.petesGuitar]
    $ investigator AshcanPeteParallel Cards.ashcanPeteParallel
    $ Stats {health = 7, sanity = 7, willpower = 4, intellect = 2, combat = 3, agility = 3}

instance HasAbilities AshcanPeteParallel where
  getAbilities (AshcanPeteParallel a) =
    [ playerLimit PerRound
        $ restrictedAbility a 1 Self
        $ freeReaction
        $ oneOf
          [ AssetWouldBeDiscarded #when (AssetControlledBy You <> AssetAttachedTo ScenarioCardTarget)
          , EventWouldBeDiscarded #when (EventControlledBy You <> EventAttachedTo ScenarioCardTarget)
          ]
    ]

instance HasChaosTokenValue AshcanPeteParallel where
  getChaosTokenValue iid ElderSign (AshcanPeteParallel attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

getWindowTarget :: [Window] -> Target
getWindowTarget [] = error "missing target"
getWindowTarget ((windowType -> WouldBeDiscarded target) : _) = target
getWindowTarget (_ : rest) = getWindowTarget rest

instance RunMessage AshcanPeteParallel where
  runMessage msg i@(AshcanPeteParallel attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getWindowTarget -> target) _ -> do
      returnToHand iid target
      pure i
    ElderSignEffect (is attrs -> True) -> do
      targets <-
        (<>)
          <$> selectTargets (assetControlledBy attrs.id <> AssetAttachedTo ScenarioCardTarget)
          <*> selectTargets (eventControlledBy attrs.id <> EventAttachedTo ScenarioCardTarget)

      when (notNull targets) do
        chooseOne attrs.id
          $ Label "Do not return any cards" []
          : [ targetLabel
              target
              [ReturnToHand attrs.id target]
            | target <- targets
            ]

      pure i
    _ -> AshcanPeteParallel <$> liftRunMessage msg attrs
