module Arkham.Treachery.Cards.WatchersGazeUnionAndDisillusion (
  watchersGazeUnionAndDisillusion,
  WatchersGazeUnionAndDisillusion (..),
)
where

import Arkham.Prelude

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WatchersGazeUnionAndDisillusion = WatchersGazeUnionAndDisillusion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchersGazeUnionAndDisillusion :: TreacheryCard WatchersGazeUnionAndDisillusion
watchersGazeUnionAndDisillusion = treachery WatchersGazeUnionAndDisillusion Cards.watchersGazeUnionAndDisillusion

instance RunMessage WatchersGazeUnionAndDisillusion where
  runMessage msg t@(WatchersGazeUnionAndDisillusion attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> do
      investigators <- getInvestigators
      pushAll [revelationSkillTest iid attrs SkillWillpower 5 | iid <- investigators]
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      yourLocationIsHaunted <- selectAny $ locationWithInvestigator iid <> HauntedLocation
      watchersLocationIsHaunted <-
        selectAny $ LocationWithEnemy (enemyIs Enemies.theSpectralWatcher) <> HauntedLocation
      when (yourLocationIsHaunted || watchersLocationIsHaunted)
        $ push
        $ chooseOrRunOne iid
        $ [ Label
            "Resolve the Haunted ability on your location"
            [HandleTargetChoice iid (toSource attrs) (toTarget iid)]
          | yourLocationIsHaunted
          ]
        <> [ Label
            "Resolve the Haunted ability on The Spectral Watcher's location"
            [HandleTargetChoice iid (toSource attrs) (toTarget attrs)]
           | yourLocationIsHaunted
           ]

      pure t
    HandleTargetChoice iid (isSource attrs -> True) (InvestigatorTarget _) -> do
      runHauntedAbilities iid
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (InvestigatorTarget _) -> do
      lid <- selectJust $ LocationWithEnemy (enemyIs Enemies.theSpectralWatcher)
      runLocationHauntedAbilities iid lid
      pure t
    _ -> WatchersGazeUnionAndDisillusion <$> runMessage msg attrs
