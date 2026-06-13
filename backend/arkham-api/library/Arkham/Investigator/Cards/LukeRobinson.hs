module Arkham.Investigator.Cards.LukeRobinson (lukeRobinson) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Cost
import Arkham.Cost.Status qualified as CostStatus
import Arkham.ForMovement
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Action (canDo_, getActions)
import Arkham.Helpers.Modifiers (
  ModifierType (..),
  getModifiers,
  modifySelf,
  toModifiers,
  withModifiers,
 )
import Arkham.Helpers.Playable (getIsPlayable, getPlayableCards)
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Runner (metaL, runWindow)
import Arkham.Matcher hiding (PlayCard)
import Arkham.Message.Lifted.Choose
import Arkham.Queue (QueueT)
import Arkham.Tracing
import Arkham.Window (Window, defaultWindows)
import Arkham.Window qualified as Window

newtype Meta = Meta {active :: Bool}
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype LukeRobinson = LukeRobinson (InvestigatorAttrs `With` Meta)
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance IsInvestigator LukeRobinson where
  investigatorFromAttrs = LukeRobinson . (`with` Meta False)

instance HasModifiersFor LukeRobinson where
  getModifiersFor (LukeRobinson (a `With` meta)) = do
    if active meta
      then do
        connectingLocations <- select (ConnectedLocation NotForMovement)
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

getLukePlayable
  :: (Tracing m, HasGame m) => InvestigatorAttrs -> [Window] -> m [(LocationId, [Card])]
getLukePlayable attrs windows' = do
  let iid = toId attrs
  connectingLocations <- select (ConnectedLocation NotForMovement)
  forToSnd connectingLocations \lid -> do
    enemies <- select $ enemyAt lid
    withoutModifiersFrom iid do
      withModifiers iid (toModifiers attrs $ AsIfAt lid : map AsIfEngagedWith enemies) do
        filter (`cardMatch` CardWithType EventType)
          <$> getPlayableCards attrs iid (UnpaidCost NeedsAction) windows'

instance RunMessage LukeRobinson where
  runMessage msg i@(LukeRobinson (attrs `With` meta)) = runQueueT $ case msg of
    ElderSignEffect iid | attrs `is` iid -> do
      mGateBox <- selectOne $ assetIs Assets.gateBox
      for_ mGateBox $ \gateBox -> push $ AddUses #elderSign gateBox Charge 1
      pure i
    PlayerWindow iid additionalActions isAdditional immediate | active meta -> do
      -- N.B. we are not checking if iid is us so we must be careful not to use it incorrectly
      let usesAction = not isAdditional
      canPlay <- canDo_ (toId attrs) #play

      if canPlay
        then do
          lukePlayable <- getLukePlayable attrs (defaultWindows iid)
          let
            asIfActions =
              [ targetLabel
                  (toCardId c)
                  [InitiatePlayCardWithWindows (toId attrs) c Nothing NoPayment (defaultWindows iid) usesAction]
              | c <- concatMap snd lukePlayable
              ]
          LukeRobinson
            . (`with` meta)
            <$> liftRunMessage (PlayerWindow iid (additionalActions <> asIfActions) isAdditional immediate) attrs
        else LukeRobinson . (`with` meta) <$> liftRunMessage msg attrs
    Do (CheckWindows windows')
      | active meta
      , not (investigatorSkippedWindow attrs)
      , attrs.inGame
          || Window.hasEliminatedWindow windows' -> do
          lukePlayable <- concatMap snd <$> getLukePlayable attrs windows'
          actions <- getActions attrs.id windows'
          playableCards <- getPlayableCards attrs attrs.id (UnpaidCost NeedsAction) windows'
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
        playCard = unless shouldSkip $ push $ PlayCard iid card mtarget payment windows' asAction

      let mCardId = maybeResult attrs.meta

      engaged <- select $ enemyEngagedWith attrs.id

      -- N.B. we derive the connecting-location options from this specific card via
      -- getIsPlayable rather than re-enumerating getLukePlayable. The latter is driven by
      -- getPlayableCards and misses events played from another zone by a third-party effect
      -- (e.g. De Vermis Mysteriis playing a Fight event from the discard), which would
      -- otherwise resolve at the current location and crash if no enemy is here. We use
      -- PaidCost because by the time we initiate the play the cost has already been settled;
      -- checking UnpaidCost here would spuriously fail affordability after resources were spent.
      connecting <- select (ConnectedLocation NotForMovement)
      lids <-
        if card `cardMatch` CardWithType EventType
          then flip filterM connecting \lid -> do
            enemies <- select $ enemyAt lid
            withoutModifiersFrom iid
              $ withModifiers iid (toModifiers attrs $ AsIfAt lid : map AsIfEngagedWith enemies)
              $ getIsPlayable iid (toSource attrs) CostStatus.PaidCost windows' card
          else pure []

      if notNull lids && Just card.id /= mCardId
        then do
          chooseOrRunOneM iid do
            when playable do
              labeledI "playAtCurrentLocation" do
                doStep 1 msg
                playCard
            when (notNull lids) do
              labeledI "playAtConnectingLocation" $ do
                handleTarget iid attrs attrs
                chooseOrRunOneM iid do
                  targets lids \lid -> do
                    enemies <- select $ enemyAt lid
                    cardResolutionModifiers card attrs attrs $ AsIfAt lid : map AsIfEngagedWith enemies <> map AsIfNotEngagedWith engaged
                    playCard
        else runQueueT playCard
      -- we unset the tracked card here, it will have entered play and should be available again
      pure $ LukeRobinson . (`with` meta) $ attrs & metaL .~ Null
    DoStep 1 (InitiatePlayCard iid card _ _ _ _) | iid == toId attrs -> do
      -- we've decided on the card so we need to track that
      pure $ LukeRobinson . (`with` meta) $ attrs & metaL .~ toJSON card.id
    HandleTargetChoice _ (isSource attrs -> True) (isTarget attrs -> True) -> do
      pure $ LukeRobinson (attrs `with` Meta False)
    EndTurn _ -> do
      attrs' <- liftRunMessage msg attrs
      pure $ LukeRobinson (attrs' `with` Meta True)
    ForInvestigators _ ResetGame -> LukeRobinson . (`with` Meta True) <$> liftRunMessage msg attrs
    _ -> LukeRobinson . (`with` meta) <$> liftRunMessage msg attrs
