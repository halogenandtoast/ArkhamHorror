{-# OPTIONS_GHC -Wno-deprecations #-}

module Arkham.Scenarios.FatalMirage.Helpers where

import Arkham.Ability
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.Query
import Arkham.Classes.RunMessage
import Arkham.Constants
import Arkham.GameT
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Helpers.Story
import Arkham.I18n
import Arkham.Location.Base (revealCluesL)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner ()
import Arkham.Location.Types (LocationAttrs)
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message (Message (Flip, PlacedLocation), is, pattern UseThisAbility)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Prelude
import Arkham.Queue
import Arkham.Source
import Arkham.Target

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "fatalMirage" a

mirage
  :: (HasCardCode a, Sourceable a, HasCardCode location) => a -> Int -> [location] -> Ability
mirage a clues locations =
  restricted a MirageAbility (Here <> SetAsideCardExists (mapOneOf cardIs locations))
    $ FastAbility
    $ GroupClueCost (PerPlayer clues) YourLocation

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
