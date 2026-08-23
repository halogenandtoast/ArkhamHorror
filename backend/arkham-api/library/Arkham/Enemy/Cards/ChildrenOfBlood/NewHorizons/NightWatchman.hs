module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.NightWatchman (nightWatchman) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (DiscoverClues)
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype NightWatchman = NightWatchman EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightWatchman :: EnemyCard NightWatchman
nightWatchman = enemy NightWatchman Cards.nightWatchman & setPrey MostClues

instance HasAbilities NightWatchman where
  getAbilities (NightWatchman a) =
    extend1 a $ mkAbility a 1 $ forced $ DiscoverClues #after Anyone Anywhere (atLeast 1)

getDiscoveredAt :: [Window] -> LocationId
getDiscoveredAt [] = error "wrong window"
getDiscoveredAt ((windowType -> Window.DiscoverClues _ lid _ _) : _) = lid
getDiscoveredAt (_ : xs) = getDiscoveredAt xs

instance RunMessage NightWatchman where
  runMessage msg e@(NightWatchman attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getDiscoveredAt -> lid) _ -> do
      if attrs.exhausted
        then ready attrs
        else do
          disengageFromAll attrs
          current <- field EnemyLocation attrs.id
          unless (current == Just lid) $ moveToward attrs (LocationWithId lid)
      pure e
    _ -> NightWatchman <$> liftRunMessage msg attrs
