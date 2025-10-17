module Arkham.Enemy.Cards.TheSanguineWatcherWithTheRubySpectacles (theSanguineWatcherWithTheRubySpectacles) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TheSanguineWatcherWithTheRubySpectacles = TheSanguineWatcherWithTheRubySpectacles EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theSanguineWatcherWithTheRubySpectacles :: EnemyCard TheSanguineWatcherWithTheRubySpectacles
theSanguineWatcherWithTheRubySpectacles =
  enemy
    TheSanguineWatcherWithTheRubySpectacles
    Cards.theSanguineWatcherWithTheRubySpectacles
    (4, PerPlayer 5, 4)
    (2, 2)

instance RunMessage TheSanguineWatcherWithTheRubySpectacles where
  runMessage msg (TheSanguineWatcherWithTheRubySpectacles attrs) = runQueueT $ case msg of
    _ -> TheSanguineWatcherWithTheRubySpectacles <$> liftRunMessage msg attrs
