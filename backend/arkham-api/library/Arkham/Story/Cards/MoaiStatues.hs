module Arkham.Story.Cards.MoaiStatues (moaiStatues) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype MoaiStatues = MoaiStatues StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moaiStatues :: StoryCard MoaiStatues
moaiStatues = story MoaiStatues Cards.moaiStatues

instance RunMessage MoaiStatues where
  runMessage msg s@(MoaiStatues attrs) = runQueueT $ case msg of
    ResolveStory _iid ResolveIt story' | story' == toId attrs -> do
      getPartnerStatus Assets.roaldEllsworthIntrepidExplorer >>= \case
        Eliminated -> do
          loc <- selectJust (locationIs Locations.moaiStatues)
          selectEach (investigatorAt loc) \investigator ->
            moveTo_ attrs investigator (locationIs Locations.prisonOfMemories)
          selectEach (oneOf [UnengagedEnemy, MassiveEnemy] <> enemyAt loc) \enemy ->
            moveTo_ attrs enemy (locationIs Locations.prisonOfMemories)
          addToVictory loc
          mayAdvance attrs
        _ ->
          getSetAsideCard Enemies.memoryOfAnAlienTranslation
            >>= (`createEnemy_` Locations.moaiStatues)
      pure s
    _ -> MoaiStatues <$> liftRunMessage msg attrs
