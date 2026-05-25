{-# OPTIONS_GHC -Wno-unused-imports #-}

module Arkham.Message.Lifted.Location where

import Arkham.Helpers.FetchCard as X
import Arkham.Message.Lifted.Card

import Arkham.Ability
import Arkham.Act.Sequence qualified as Act
import Arkham.Act.Types (ActAttrs (actDeckId))
import Arkham.Action (Action)
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Agenda.Types (AgendaAttrs (agendaDeckId))
import Arkham.Aspect (IsAspect (..))
import Arkham.Aspect qualified as Msg
import Arkham.Asset.Types (AssetAttrs)
import Arkham.Asset.Types qualified as Field
import Arkham.Asset.Uses (UseType)
import Arkham.Attack
import Arkham.Calculation
import Arkham.CampaignStep hiding (continue)
import Arkham.CampaignStep qualified as CS
import Arkham.Campaigns.TheScarletKeys.Key.Id
import Arkham.Capability
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.ChaosToken
import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue hiding (insertAfterMatching)
import Arkham.Classes.HasQueue as X (runQueueT)
import Arkham.Classes.Query
import Arkham.DamageEffect
import Arkham.Deck (IsDeck (..))
import Arkham.Deck qualified as Deck
import Arkham.Discover as X (IsInvestigate (..))
import Arkham.Discover qualified as Msg
import Arkham.Draw.Types
import Arkham.Effect.Builder
import Arkham.Effect.Types (EffectBuilder (effectBuilderEffectId), Field (..))
import Arkham.Effect.Window
import Arkham.EffectMetadata (EffectMetadata)
import Arkham.EncounterSet
import Arkham.Enemy.Creation
import Arkham.Enemy.Helpers qualified as Msg
import Arkham.Enemy.Types (Field (..))
import Arkham.Evade
import Arkham.Evade qualified as Evade
import Arkham.Exhaust qualified as Exhaust
import Arkham.Fight
import Arkham.Fight qualified as Fight
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Act
import Arkham.Helpers.Agenda
import Arkham.Helpers.Campaign
import Arkham.Helpers.Campaign qualified as Msg
import Arkham.Helpers.Card (getCardEntityTarget)
import Arkham.Helpers.ChaosToken qualified as Msg
import Arkham.Helpers.Effect qualified as Msg
import Arkham.Helpers.Enemy qualified as Msg
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Investigator (
  canHaveDamageHealed,
  canHaveHorrorHealed,
  getCanDiscoverClues,
 )
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Message.Discard qualified as HandDiscard
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.Query
import Arkham.Helpers.Ref (sourceToTarget)
import Arkham.Helpers.Scenario (getEncounterDeckKey, getInResolution, getIsStandalone)
import Arkham.Helpers.Shuffle
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.UI qualified as Msg
import Arkham.Helpers.Window qualified as Msg
import Arkham.Helpers.Xp
import Arkham.History
import Arkham.I18n
import Arkham.Id
import Arkham.Investigate
import Arkham.Investigate qualified as Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Layout
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..), Location)
import Arkham.Matcher hiding (PerformAction)
import Arkham.Message hiding (story)
import Arkham.Message as X (AndThen (..), getChoiceAmount, optionWhenExists, preOriginalOption)
import Arkham.Message.Lifted.Base
import Arkham.Message.Lifted.Queue as X
import Arkham.Modifier
import Arkham.Name
import Arkham.Phase (Phase)
import Arkham.Placement
import Arkham.Prelude hiding (pred)
import Arkham.Projection
import Arkham.Query
import Arkham.Queue
import Arkham.RequestedChaosTokenStrategy
import Arkham.Scenario.Deck
import Arkham.Search qualified as Search
import Arkham.SkillTest.Step
import Arkham.SkillType
import Arkham.SkillType qualified as SkillType
import Arkham.Source
import Arkham.Spawn
import Arkham.Target
import Arkham.Token
import Arkham.Tracing
import Arkham.Trait (Trait)
import Arkham.Window (Window (..), WindowType, defaultWindows)
import Arkham.Window qualified as Window
import Arkham.Xp
import Control.Monad.State.Strict (MonadState, StateT, execStateT, get, put)
import Control.Monad.Trans.Class
import Data.Aeson.Key qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Typeable

placeSetAsideLocation :: ReverseQueue m => CardDef -> m LocationId
placeSetAsideLocation card = do
  (lid, msg) <- Msg.placeSetAsideLocation card
  push msg
  pure lid

selectOrPlaceSetAsideLocation :: ReverseQueue m => CardDef -> m LocationId
selectOrPlaceSetAsideLocation def = maybe (placeSetAsideLocation def) pure =<< selectOne (locationIs def)

placeSetAsideLocation_ :: ReverseQueue m => CardDef -> m ()
placeSetAsideLocation_ = push <=< Msg.placeSetAsideLocation_

placeSetAsideLocationWith_ :: ReverseQueue m => CardDef -> Update Location -> m ()
placeSetAsideLocationWith_ def = push <=< Msg.placeSetAsideLocationWith_ def

placeSetAsideLocations_ :: ReverseQueue m => [CardDef] -> m ()
placeSetAsideLocations_ = pushAll <=< Msg.placeSetAsideLocations

placeSetAsideLocationsMatching_ :: ReverseQueue m => CardMatcher -> m ()
placeSetAsideLocationsMatching_ matcher = getSetAsideCardsMatching (#location <> matcher) >>= placeSetAsideLocations_ . map toCardDef

placeLocationCard :: ReverseQueue m => CardDef -> m LocationId
placeLocationCard def = do
  (lid, placement) <- Msg.placeLocationCard def
  push placement
  pure lid

placeLocationCardInGrid :: ReverseQueue m => Pos -> CardDef -> m LocationId
placeLocationCardInGrid pos def = do
  (lid, placement) <- Msg.placeLocationCardInGrid pos def
  push placement
  pure lid

placeLocationCardInGrid_ :: ReverseQueue m => Pos -> CardDef -> m ()
placeLocationCardInGrid_ pos def = void $ placeLocationCardInGrid pos def

placeLocationInGrid :: ReverseQueue m => Pos -> Card -> m LocationId
placeLocationInGrid pos card = do
  (lid, placement) <- Msg.placeLocationInGrid pos card
  push placement
  pure lid

placeLocationInGrid_ :: ReverseQueue m => Pos -> Card -> m ()
placeLocationInGrid_ pos card = void $ placeLocationInGrid pos card

placeLocation :: (ReverseQueue m, FetchCard card) => card -> m LocationId
placeLocation card = do
  (lid, placement) <- Msg.placeLocation =<< fetchCard card
  push placement
  pure lid

placeLocation_ :: (ReverseQueue m, FetchCard card) => card -> m ()
placeLocation_ = fetchCard >=> Msg.placeLocation_ >=> push

placeLocationIfNotInPlay_ :: (HasCallStack, ReverseQueue m) => CardDef -> m ()
placeLocationIfNotInPlay_ = void . placeLocationIfNotInPlay

placeLocationIfNotInPlay :: (HasCallStack, ReverseQueue m) => CardDef -> m LocationId
placeLocationIfNotInPlay def =
  selectOne (locationIs def) >>= \case
    Just lid -> pure lid
    Nothing ->
      getSetAsideCardMaybe def >>= \case
        Nothing -> error $ "Location not found in play or set aside: " <> show def
        Just card -> placeLocation card

placeRandomLocationGroupCards :: ReverseQueue m => Text -> [CardDef] -> m ()
placeRandomLocationGroupCards groupName = traverse fetchCard >=> placeRandomLocationGroup groupName

placeRandomLocationGroupCardsCapture :: ReverseQueue m => Text -> [CardDef] -> m [LocationId]
placeRandomLocationGroupCardsCapture groupName = genCards >=> placeRandomLocationGroupCapture groupName

placeRandomLocationGroup :: ReverseQueue m => Text -> [Card] -> m ()
placeRandomLocationGroup groupName cards = do
  shuffled <- shuffleM cards
  msgs <- Msg.placeLabeledLocations_ groupName shuffled
  pushAll msgs

placeRandomLocationGroupCapture :: ReverseQueue m => Text -> [Card] -> m [LocationId]
placeRandomLocationGroupCapture groupName cards = do
  shuffled <- shuffleM cards
  (lids, msgs) <- Msg.placeLabeledLocations groupName shuffled
  pushAll msgs
  pure lids

placeLabeledLocations_ :: ReverseQueue m => Text -> [Card] -> m ()
placeLabeledLocations_ lbl cards = Msg.pushAllM $ Msg.placeLabeledLocations_ lbl cards

placeLabeledLocations :: ReverseQueue m => Text -> [Card] -> m [LocationId]
placeLabeledLocations lbl cards = Msg.placeLabeledLocations lbl cards >>= \(lids, msgs) -> pushAll msgs $> lids

placeLabeledLocation_ :: ReverseQueue m => Text -> Card -> m ()
placeLabeledLocation_ lbl card = Msg.placeLabeledLocation lbl card >>= \(_, msg) -> push msg

placeLabeledLocation :: ReverseQueue m => Text -> Card -> m LocationId
placeLabeledLocation lbl card = Msg.placeLabeledLocation lbl card >>= \(lid, msg) -> push msg >> pure lid

placeLabeledLocationsFrom :: ReverseQueue m => Text -> Int -> [Card] -> m [LocationId]
placeLabeledLocationsFrom lbl n cards = Msg.placeLabeledLocationsFrom lbl n cards >>= \(lids, msgs) -> pushAll msgs $> lids

placeLocationCards :: ReverseQueue m => [CardDef] -> m ()
placeLocationCards defs = for_ defs placeLocationCard

placeOneLocationCard :: ReverseQueue m => NonEmpty CardDef -> m LocationId
placeOneLocationCard = sample >=> placeLocationCard

placeLocationCardM :: ReverseQueue m => m CardDef -> m LocationId
placeLocationCardM = (>>= placeLocationCard)

reveal :: (AsId location, IdOf location ~ LocationId, ReverseQueue m) => location -> m ()
reveal (asId -> lid) = do
  inSetup <- getInSetup
  if inSetup
    then unsafeReveal lid
    else whenMatch lid UnrevealedLocation $ unsafeReveal lid

unsafeReveal :: (AsId location, IdOf location ~ LocationId, ReverseQueue m) => location -> m ()
unsafeReveal (asId -> lid) = push $ Msg.RevealLocation Nothing lid

revealMatching :: ReverseQueue m => LocationMatcher -> m ()
revealMatching matcher = selectEach matcher (push . Msg.RevealLocation Nothing)

eachLocation :: ReverseQueue m => (LocationId -> m ()) -> m ()
eachLocation = selectEach Anywhere

removeLocation :: (ReverseQueue m, AsId location, IdOf location ~ LocationId) => location -> m ()
removeLocation (asId -> lid) = do
  whenM (matches lid $ IncludeEmptySpace $ not_ LocationBeingRemoved) do
    noClues <- lid <=~> LocationWithoutClues
    if noClues
      then
        maybe (pushAll $ resolve (RemoveLocation lid)) (\_ -> addToVictory_ lid)
          =<< field LocationVictory lid
      else pushAll $ resolve (RemoveLocation lid)

setLocationLabel :: (ToId location LocationId, ReverseQueue m) => location -> Text -> m ()
setLocationLabel location lbl = push $ SetLocationLabel (asId location) lbl
