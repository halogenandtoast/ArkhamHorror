module Arkham.Investigator.Cards.TonyMorgan (
  tonyMorgan,
  TonyMorgan (..),
)
where

import Arkham.Prelude

import Arkham.Action.Additional
import Arkham.Card
import Arkham.Game.Helpers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Window (defaultWindows)
import Control.Lens (over)
import Data.Data.Lens (biplate)

newtype Meta = Meta {active :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TonyMorgan = TonyMorgan (InvestigatorAttrs `With` Meta)
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tonyMorgan :: InvestigatorCard TonyMorgan
tonyMorgan =
  investigator
    (TonyMorgan . (`with` Meta False))
    Cards.tonyMorgan
    Stats
      { health = 9
      , sanity = 5
      , willpower = 2
      , intellect = 3
      , combat = 5
      , agility = 2
      }

instance HasModifiersFor TonyMorgan where
  getModifiersFor target (TonyMorgan (a `With` meta)) | a `is` target = do
    pure $ toModifiers a $ GiveAdditionalAction BountyAction : [BountiesOnly | active meta]
  getModifiersFor _ _ = pure []

instance HasAbilities TonyMorgan where
  getAbilities (TonyMorgan (attrs `With` _)) =
    [ doesNotProvokeAttacksOfOpportunity
      $ restrictedAbility
        attrs
        1
        (Self <> exists (EnemyWithBounty <> EnemyOneOf [CanFightEnemy (toSource attrs), CanEngageEnemy]))
      $ ActionAbility Nothing mempty
    | BountyAction `elem` investigatorAdditionalActions attrs
    ]

instance HasChaosTokenValue TonyMorgan where
  getChaosTokenValue iid ElderSign (TonyMorgan (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage TonyMorgan where
  runMessage msg (TonyMorgan (attrs `With` meta)) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      modifiers <- getModifiers iid
      let windows' = defaultWindows iid

      actions <- withModifiers attrs (toModifiers attrs [ActionCostModifier (-1), BountiesOnly]) $ do
        map (over biplate (`decreaseActionCost` 1))
          . filter ((`elem` [Just #fight, Just #engage]) . abilityAction)
          . nub
          <$> concatMapM (getActions iid) windows'
      playableCards <- withModifiers attrs (toModifiers attrs [ActionCostModifier (-1), BountiesOnly]) $ do
        filter (any (`elem` [#fight, #engage]) . cdActions . toCardDef)
          <$> getPlayableCards attrs UnpaidCost windows'

      let
        canDo action = not <$> anyM (prevents action) modifiers
        prevents action = \case
          CannotTakeAction x -> preventsAction action x
          MustTakeAction x -> not <$> preventsAction action x -- reads a little weird but we want only thing things x would prevent with cannot take action
          _ -> pure False
        preventsAction action = \case
          FirstOneOfPerformed as | action `elem` as -> do
            fieldP InvestigatorActionsPerformed (\taken -> all (`notElem` taken) as) iid
          FirstOneOfPerformed {} -> pure False
          IsAction action' -> pure $ action == action'
          EnemyAction {} -> pure False

      canPlay <- canDo #play

      push
        $ AskPlayer
        $ chooseOne iid
        $ [ targetLabel (toCardId c) [InitiatePlayCard iid c Nothing windows' False]
          | canPlay
          , c <- playableCards
          ]
        <> map ((\f -> f windows' []) . AbilityLabel iid) actions
      pure $ TonyMorgan . (`with` Meta True) $ attrs & additionalActionsL %~ deleteFirst BountyAction
    ChooseFightEnemy iid source mTarget skillType enemyMatcher isAction | iid == toId attrs -> do
      bountiesOnly <- hasModifier iid BountiesOnly
      let matcherF = if bountiesOnly then (<> EnemyWithBounty) else id
      result <-
        runMessage
          (ChooseFightEnemy iid source mTarget skillType (matcherF enemyMatcher) isAction)
          attrs
      pure $ TonyMorgan . (`with` Meta False) $ result
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      pure $ TonyMorgan $ attrs `with` Meta False
    _ -> TonyMorgan . (`with` meta) <$> runMessage msg attrs
