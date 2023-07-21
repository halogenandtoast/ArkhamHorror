module Arkham.Treachery.Cards.WatchersGazeUnionAndDisillusion (
  watchersGazeUnionAndDisillusion,
  WatchersGazeUnionAndDisillusion (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WatchersGazeUnionAndDisillusion = WatchersGazeUnionAndDisillusion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchersGazeUnionAndDisillusion :: TreacheryCard WatchersGazeUnionAndDisillusion
watchersGazeUnionAndDisillusion = treachery WatchersGazeUnionAndDisillusion Cards.watchersGazeUnionAndDisillusion

instance RunMessage WatchersGazeUnionAndDisillusion where
  runMessage msg t@(WatchersGazeUnionAndDisillusion attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> WatchersGazeUnionAndDisillusion <$> runMessage msg attrs
