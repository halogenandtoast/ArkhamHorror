module Arkham.Investigator.Cards.LukeRobinson (lukeRobinson, LukeRobinson (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Cost
import Arkham.Game.Helpers (canDo, getActions, getIsPlayable, getPlayableCards)
import Arkham.Helpers.Modifiers (
  ModifierType (..),
  getModifiers,
  modifySelf,
  toModifiers,
  withModifiers,
 )
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Runner (runWindow)
import Arkham.Matcher hiding (PlayCard)
import Arkham.Message.Lifted.Choose
import Arkham.Queue (QueueT)
import Arkham.Window (Window, defaultWindows)
import Arkham.Window qualified as Window

newtype Meta = Meta {active :: Bool}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype LukeRobinson = LukeRobinson (InvestigatorAttrs `With` Meta)
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance HasModifiersFor LukeRobinson where
  getModifiersFor (LukeRobinson (a `With` meta)) = do
    if active meta
      then do
        connectingLocations <- select ConnectedLocation
        mods <- for connectingLocations $ \lid -> do
          enemies <- select $ enemyAt lid
          pure (AsIfAt lid : map AsIfEngagedWith enemies)
        modifySelf a [PlayableModifierContexts $ map (CardWithType EventType,) mods]
      else pure mempty

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
        <$> getPlayableCards attrs (UnpaidCost NeedsAction) windows'

instance RunMessage LukeRobinson where
  runMessage msg i@(LukeRobinson (attrs `With` meta)) = runQueueT $ case msg of
    ElderSignEffect iid | attrs `is` iid -> do
      gateBox <- selectJust $ assetIs Assets.gateBox
      push $ AddUses #elderSign gateBox Charge 1
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
            <$> liftRunMessage (PlayerWindow iid (additionalActions <> asIfActions) isAdditional) attrs
        else LukeRobinson . (`with` meta) <$> liftRunMessage msg attrs
    Do (CheckWindows windows')
      | active meta
      , not (investigatorSkippedWindow attrs)
      , attrs.inGame
          || Window.hasEliminatedWindow windows' -> do
          lukePlayable <- concatMap snd <$> getLukePlayable attrs windows'
          actions <- getActions attrs.id windows'
          playableCards <- getPlayableCards attrs (UnpaidCost NeedsAction) windows'
          runWindow attrs windows' actions (nub $ playableCards <> lukePlayable)
          pure i
    InitiatePlayCard iid card mtarget payment windows' asAction | iid == toId attrs && active meta -> do
      let a = attrs
      mods <- getModifiers (toTarget a)
      playable <- withModifiers iid (toModifiers attrs [IgnorePlayableModifierContexts]) $ do
        getIsPlayable (toId a) (toSource a) (UnpaidCost NeedsAction) windows' card
      let
        shouldSkip = flip any mods $ \case
          AsIfInHand card' -> card == card'
          _ -> False

      let
        playCard :: ReverseQueue m => QueueT Message m ()
        playCard = when (not shouldSkip) do
          checkWhen $ Window.PlayCard iid $ Window.CardPlay card asAction
          push $ PlayCard iid card mtarget payment windows' asAction
          checkAfter $ Window.PlayCard iid $ Window.CardPlay card asAction

      lukePlayable <- getLukePlayable attrs windows'

      if card `elem` concatMap snd lukePlayable
        then do
          let lids = map fst $ filter (elem card . snd) lukePlayable
          chooseOrRunOneM iid do
            when playable do
              labeled "Play at current location" playCard
            when (notNull lids) do
              labeled "Play at connecting location" $ do
                handleTarget iid attrs attrs
                chooseOrRunOneM iid do
                  targets lids \lid -> do
                    enemies <- select $ enemyAt lid
                    cardResolutionModifiers card attrs attrs $ AsIfAt lid : map AsIfEngagedWith enemies
                    playCard
        else runQueueT playCard
      pure i
    HandleTargetChoice _ (isSource attrs -> True) (isTarget attrs -> True) -> do
      pure $ LukeRobinson (attrs `with` Meta False)
    EndTurn _ -> do
      attrs' <- liftRunMessage msg attrs
      pure $ LukeRobinson (attrs' `with` Meta True)
    ResetGame -> LukeRobinson . (`with` Meta True) <$> liftRunMessage msg attrs
    _ -> LukeRobinson . (`with` meta) <$> liftRunMessage msg attrs
