module Arkham.Treachery.Cards.WatchersGrasp (watchersGrasp, WatchersGrasp (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WatchersGrasp = WatchersGrasp TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchersGrasp :: TreacheryCard WatchersGrasp
watchersGrasp = treachery WatchersGrasp Cards.watchersGrasp

instance HasModifiersFor WatchersGrasp where
  getModifiersFor (EnemyTarget eid) (WatchersGrasp a) = do
    isTheSpectralWatcher <- eid <=~> enemyIs Enemies.theSpectralWatcher
    pure
      $ toModifiers
        a
        [ ForcePrey $ Prey $ InvestigatorWithId $ treacheryDrawnBy a
        | isTheSpectralWatcher
        ]
  getModifiersFor _ _ = pure []

instance RunMessage WatchersGrasp where
  runMessage msg t@(WatchersGrasp attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> do
      theSpectralWatcher <- selectJust (enemyIs Enemies.theSpectralWatcher)

      pushAll
        [ HealDamage (toTarget theSpectralWatcher) (toSource attrs) 3
        , Ready (toTarget theSpectralWatcher)
        , SendMessage (toTarget theSpectralWatcher) HuntersMove
        , SendMessage (toTarget theSpectralWatcher) EnemiesAttack
        ]
      pure t
    _ -> WatchersGrasp <$> runMessage msg attrs
