module Arkham.Investigator.Cards.LukeRobinson (
  lukeRobinson,
  LukeRobinson (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Card
import {-# SOURCE #-} Arkham.Game
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (PlayCard)
import Arkham.Movement
import Arkham.Projection
import Arkham.Window (defaultWindows, mkAfter, mkWhen)
import Arkham.Window qualified as Window
import Data.Map.Strict qualified as Map

newtype Meta = Meta {active :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LukeRobinson = LukeRobinson (InvestigatorAttrs `With` Meta)
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lukeRobinson :: InvestigatorCard LukeRobinson
lukeRobinson =
  startsWith [Assets.gateBox]
    $ investigator (LukeRobinson . (`with` Meta True)) Cards.lukeRobinson
    $ Stats {health = 5, sanity = 9, willpower = 4, intellect = 3, combat = 2, agility = 3}

instance HasModifiersFor LukeRobinson where
  getModifiersFor target (LukeRobinson (attrs `With` meta)) | attrs `is` target = do
    pure $ toModifiers attrs [IsLuke | active meta]
  getModifiersFor _ _ = pure []

instance HasChaosTokenValue LukeRobinson where
  getChaosTokenValue iid ElderSign (LukeRobinson (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

getLukePlayable :: InvestigatorAttrs -> GameT [(LocationId, [Card])]
getLukePlayable attrs = do
  let iid = toId attrs
  connectingLocations <- selectList $ ConnectedLocation
  currentEnemies <- selectList $ enemyEngagedWith iid
  forToSnd connectingLocations $ \lid -> asIfGame $ do
    enemies <- selectList $ enemyAt lid
    pushAll
      $ map (DisengageEnemy iid) currentEnemies
      <> [MoveTo (move GameSource iid lid)]
      <> [EngageEnemy iid eid False | eid <- enemies]
    runMessages Nothing
    filter (`cardMatch` CardWithType EventType)
      <$> getPlayableCards attrs UnpaidCost (defaultWindows iid)

instance RunMessage LukeRobinson where
  runMessage msg i@(LukeRobinson (attrs `With` meta)) = case msg of
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      gateBox <- selectJust $ assetIs Assets.gateBox
      push $ AddUses gateBox Charge 1
      pure i
    PlayerWindow iid additionalActions isAdditional | iid == toId attrs -> do
      modifiers <- getModifiers iid
      let
        usesAction = not isAdditional
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

      if active meta && canPlay
        then do
          connectingLocations <- selectList $ ConnectedLocation
          lukePlayable <- getLukePlayable attrs
          let
            asIfActions =
              [ targetLabel (toCardId c) [InitiatePlayCard iid c Nothing (defaultWindows iid) usesAction]
              | c <- concatMap snd lukePlayable
              ]
          LukeRobinson
            . (`with` meta)
            <$> runMessage (PlayerWindow iid (additionalActions <> asIfActions) isAdditional) attrs
        else LukeRobinson . (`with` meta) <$> runMessage msg attrs
    PlayerWindow iid additionalActions isAdditional | iid /= toId attrs -> do
      LukeRobinson . (`with` meta) <$> runMessage msg attrs
    RunWindow iid windows
      | iid == toId attrs
      , ( not (investigatorDefeated attrs || investigatorResigned attrs) || Window.hasEliminatedWindow windows
        ) -> do
          LukeRobinson . (`with` meta) <$> runMessage msg attrs
    InitiatePlayCard iid card mtarget windows' asAction | iid == toId attrs -> do
      let a = attrs
      modifiers' <- getModifiers (toTarget a)
      playable <- getIsPlayable (toId a) (toSource a) UnpaidCost (defaultWindows iid) card
      let
        shouldSkip = flip any modifiers' $ \case
          AsIfInHand card' -> card == card'
          _ -> False
      playCard <-
        if shouldSkip
          then pure []
          else do
            afterPlayCard <- checkWindows [mkAfter (Window.PlayCard iid card)]
            pure
              $ [ CheckWindow [iid] [mkWhen (Window.PlayCard iid card)]
                , PlayCard iid card mtarget windows' asAction
                , afterPlayCard
                ]

      lukePlayable <- traceShowId <$> getLukePlayable attrs

      if card `elem` concatMap snd lukePlayable
        then do
          currentLocation <- getJustLocation iid
          currentEnemies <- selectList $ enemyEngagedWith iid
          let lids = map fst $ filter (elem card . snd) lukePlayable
          locationOptions <- forToSnd lids $ \lid -> do
            enemies <- selectList $ enemyAt lid
            pure
              $ map (DisengageEnemy iid) currentEnemies
              <> [SetLocationAsIf iid lid, SetEngagedAsIf iid enemies]
              <> playCard
              <> [SetLocationAsIf iid currentLocation, SetEngagedAsIf iid currentEnemies]

          push
            $ chooseOrRunOne iid
            $ [Label "Play at current location" playCard | playable]
            <> [ Label
                "Play at connecting location"
                [chooseOrRunOne iid [targetLabel lid msgs | (lid, msgs) <- locationOptions]]
               | notNull locationOptions
               ]
        else pushAll playCard
      pure i
    _ -> LukeRobinson . (`with` meta) <$> runMessage msg attrs
