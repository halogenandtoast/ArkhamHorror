module Arkham.Story.Cards.CrypticSouls (
  CrypticSouls (..),
  crypticSouls,
) where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype CrypticSouls = CrypticSouls StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticSouls :: StoryCard CrypticSouls
crypticSouls = story CrypticSouls Cards.crypticSouls

instance RunMessage CrypticSouls where
  runMessage msg s@(CrypticSouls attrs) = case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      catsCollectedTheirTribute <- getHasRecord TheCatsCollectedTheirTributeFromTheZoogs
      when catsCollectedTheirTribute
        $ pushAll [SetActions iid (toSource attrs) 0, ChooseEndTurn iid]

      forcedTheirWay <- getHasRecord TheInvestigatorsForcedTheirWayIntoTheTemple

      when forcedTheirWay $ do
        catsOfUlthar <- getSetAsideCard Enemies.catsOfUlthar
        createCatsOfUlthar <- createEnemy catsOfUlthar iid
        push createCatsOfUlthar

      pure s
    _ -> CrypticSouls <$> runMessage msg attrs
