module Arkham.Story.Cards.CrypticSouls (crypticSouls) where

import Arkham.Campaigns.TheDreamEaters.Key
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype CrypticSouls = CrypticSouls StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticSouls :: StoryCard CrypticSouls
crypticSouls = story CrypticSouls Cards.crypticSouls

instance RunMessage CrypticSouls where
  runMessage msg s@(CrypticSouls attrs) = runQueueT $ case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      catsCollectedTheirTribute <- getHasRecord TheCatsCollectedTheirTributeFromTheZoogs
      when catsCollectedTheirTribute do
        setActions iid attrs 0
        endYourTurn iid

      forcedTheirWay <- getHasRecord TheInvestigatorsForcedTheirWayIntoTheTemple

      when forcedTheirWay $ createEnemyCard_ Enemies.catsOfUlthar iid

      pure s
    _ -> CrypticSouls <$> liftRunMessage msg attrs
