{-# OPTIONS_GHC -Wno-deprecations #-}

module Arkham.Scenarios.FatalMirage.Helpers where

import Arkham.Ability
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Classes.RunMessage
import Arkham.Constants
import Arkham.GameT
import Arkham.GameValue
import Arkham.Helpers.Modifiers (modifySelfWhenM)
import Arkham.Helpers.Query
import Arkham.Helpers.Story
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Base (revealCluesL)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner ()
import Arkham.Location.Types (LocationAttrs)
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message (Message (Flip, PlacedLocation), is, pattern UseThisAbility)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Queue
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Arkham.Treachery.Cards qualified as Treacheries
import Control.Monad.Writer.Class
import Data.Map.Monoidal.Strict

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "fatalMirage" a

pattern ClearedOfMirages :: ModifierType
pattern ClearedOfMirages <- ScenarioModifier "cleared_of_mirages"
  where
    ClearedOfMirages = ScenarioModifier "cleared_of_mirages"

pattern LocationClearedOfMirages :: LocationMatcher
pattern LocationClearedOfMirages <- LocationWithModifier (ScenarioModifier "cleared_of_mirages")
  where
    LocationClearedOfMirages = LocationWithModifier (ScenarioModifier "cleared_of_mirages")

mirage
  :: (HasCardCode a, Sourceable a, HasCardCode location, AsId a, IdOf a ~ LocationId)
  => a
  -> Int
  -> [location]
  -> Ability
mirage a clues locations =
  restricted a MirageAbility (Here <> SetAsideCardExists (mapOneOf cardIs locations))
    $ FastAbility
    $ CostWhenTreacheryElse
      (TreacheryAt (LocationWithId $ asId a) <> treacheryIs Treacheries.evanescentMist)
      ( OrCost
          [ GroupClueCost (StaticWithPerPlayer 2 clues) YourLocation
          , InvestigatorDamageCost (toSource a) (at_ YourLocation) DamageAny 2
          ]
      )
      (GroupClueCost (PerPlayer clues) YourLocation)

mirageRunner
  :: CardDef -> [CardDef] -> Int -> Message -> LocationAttrs -> QueueT Message GameT LocationAttrs
mirageRunner storyCard mirageCards m msg attrs = case msg of
  UseThisAbility iid (isSource attrs -> True) MirageAbility -> do
    flipOverBy iid (attrs.ability MirageAbility) attrs
    pure attrs
  Flip iid _ (isTarget attrs -> True) -> do
    readStory iid attrs storyCard
    pure attrs
  PlacedLocation _ _ (is attrs -> True) -> do
    n <- length <$> mapMaybeM getSetAsideCardMaybe mirageCards
    liftRunMessage msg (attrs & revealCluesL .~ PerPlayer (m * n))
  _ -> liftRunMessage msg attrs

mayAdvance :: (ReverseQueue m, Sourceable source) => source -> m ()
mayAdvance source = do
  lead <- getLead
  chooseOneM lead do
    labeled "Advance the current act" $ advanceCurrentAct source
    labeled "Keep playing" nothing

handleMemory
  :: (ReverseQueue m, Sourceable source) => source -> CardDef -> CardDef -> CardDef -> m ()
handleMemory source partner location memory = do
  getPartnerStatus partner >>= \case
    Eliminated -> do
      loc <- selectJust (locationIs location)
      selectEach (investigatorAt loc) \investigator ->
        moveTo_ source investigator (locationIs Locations.prisonOfMemories)
      selectEach (oneOf [UnengagedEnemy, MassiveEnemy] <> enemyAt loc) \enemy ->
        moveTo_ source enemy (locationIs Locations.prisonOfMemories)
      addToVictory loc
      mayAdvance source
    _ -> getSetAsideCard memory >>= (`createEnemy_` location)

clearedOfMirages
  :: (HasCallStack, HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)
  => LocationAttrs
  -> [CardDef]
  -> m ()
clearedOfMirages a mirageCards =
  modifySelfWhenM
    a
    (selectNone $ SetAsideCardMatch $ cardsAre mirageCards)
    [ClearedOfMirages]
