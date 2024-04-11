module Arkham.Investigator.Cards.LukeRobinson (
  lukeRobinson,
  LukeRobinson (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (PlayCard)
import Arkham.Window (Window, defaultWindows, mkAfter, mkWhen)
import Arkham.Window qualified as Window

newtype Meta = Meta {active :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LukeRobinson = LukeRobinson (InvestigatorAttrs `With` Meta)
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LukeRobinson where
  getModifiersFor target (LukeRobinson (a `With` meta)) | a `is` target && active meta = do
    connectingLocations <- select ConnectedLocation
    mods <- for connectingLocations $ \lid -> do
      enemies <- select $ enemyAt lid
      pure (AsIfAt lid : map AsIfEngagedWith enemies)
    pure $ toModifiers a [PlayableModifierContexts $ map (CardWithType EventType,) mods]
  getModifiersFor _ _ = pure []

lukeRobinson :: InvestigatorCard LukeRobinson
lukeRobinson =
  startsWith [Assets.gateBox]
    $ investigator (LukeRobinson . (`with` Meta True)) Cards.lukeRobinson
    $ Stats {health = 5, sanity = 9, willpower = 4, intellect = 3, combat = 2, agility = 3}

instance HasChaosTokenValue LukeRobinson where
  getChaosTokenValue iid ElderSign (LukeRobinson (attrs `With` _)) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

getLukePlayable :: HasGame m => InvestigatorAttrs -> [Window] -> m [(LocationId, [Card])]
getLukePlayable attrs windows' = do
  let iid = toId attrs
  connectingLocations <- select ConnectedLocation
  forToSnd connectingLocations $ \lid -> do
    enemies <- select $ enemyAt lid
    withModifiers iid (toModifiers attrs $ AsIfAt lid : map AsIfEngagedWith enemies) $ do
      filter (`cardMatch` CardWithType EventType)
        <$> getPlayableCards attrs UnpaidCost windows'

instance RunMessage LukeRobinson where
  runMessage msg i@(LukeRobinson (attrs `With` meta)) = case msg of
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      gateBox <- selectJust $ assetIs Assets.gateBox
      push $ AddUses gateBox Charge 1
      pure i
    PlayerWindow iid additionalActions isAdditional | active meta -> do
      -- N.B. we are not checking if iid is us so we must be careful not to use it incorrectly
      let usesAction = not isAdditional
      canPlay <- canDo (toId attrs) #play

      if canPlay
        then do
          lukePlayable <- getLukePlayable attrs (defaultWindows iid)
          let
            asIfActions =
              [ targetLabel
                (toCardId c)
                [InitiatePlayCard (toId attrs) c Nothing NoPayment (defaultWindows iid) usesAction]
              | c <- concatMap snd lukePlayable
              ]
          LukeRobinson
            . (`with` meta)
            <$> runMessage (PlayerWindow iid (additionalActions <> asIfActions) isAdditional) attrs
        else LukeRobinson . (`with` meta) <$> runMessage msg attrs
    RunWindow iid windows'
      | iid == toId attrs
      , active meta
      , not (investigatorDefeated attrs || investigatorResigned attrs)
          || Window.hasEliminatedWindow windows' ->
          do
            lukePlayable <- concatMap snd <$> getLukePlayable attrs windows'
            actions <- getActions iid windows'
            playableCards <- getPlayableCards attrs UnpaidCost windows'
            runWindow attrs windows' actions (nub $ playableCards <> lukePlayable)
            pure i
    InitiatePlayCard iid card mtarget payment windows' asAction | iid == toId attrs && active meta -> do
      let a = attrs
      mods <- getModifiers (toTarget a)
      playable <- withModifiers iid (toModifiers attrs [IgnorePlayableModifierContexts]) $ do
        getIsPlayable (toId a) (toSource a) UnpaidCost windows' card
      let
        shouldSkip = flip any mods $ \case
          AsIfInHand card' -> card == card'
          _ -> False

      afterPlayCard <- checkWindows [mkAfter (Window.PlayCard iid card)]
      let playCard =
            guard (not shouldSkip)
              *> [ CheckWindow [iid] [mkWhen (Window.PlayCard iid card)]
                 , PlayCard iid card mtarget payment windows' asAction
                 , afterPlayCard
                 ]

      lukePlayable <- getLukePlayable attrs windows'

      if card `elem` concatMap snd lukePlayable
        then do
          let lids = map fst $ filter (elem card . snd) lukePlayable
          locationOptions <- forToSnd lids $ \lid -> do
            enemies <- select $ enemyAt lid
            pure
              $ [cardResolutionModifiers card attrs attrs $ AsIfAt lid : map AsIfEngagedWith enemies]
              <> playCard

          player <- getPlayer iid
          push
            $ chooseOrRunOne player
            $ [Label "Play at current location" playCard | playable]
            <> [ Label
                "Play at connecting location"
                [ HandleTargetChoice iid (toSource attrs) (toTarget attrs)
                , chooseOrRunOne player [targetLabel lid msgs | (lid, msgs) <- locationOptions]
                ]
               | notNull locationOptions
               ]
        else pushAll playCard
      pure i
    HandleTargetChoice _ (isSource attrs -> True) (isTarget attrs -> True) -> do
      pure $ LukeRobinson (attrs `with` Meta False)
    EndTurn _ -> do
      pure $ LukeRobinson (attrs `with` Meta True)
    _ -> LukeRobinson . (`with` meta) <$> runMessage msg attrs
