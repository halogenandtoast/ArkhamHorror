module Arkham.Treachery.Cards.WatchersGrasp (
  watchersGrasp,
  WatchersGrasp (..),
) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype WatchersGrasp = WatchersGrasp TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchersGrasp :: TreacheryCard WatchersGrasp
watchersGrasp = treachery WatchersGrasp Cards.watchersGrasp

instance HasModifiersFor WatchersGrasp where
  getModifiersFor (EnemyTarget eid) (WatchersGrasp a) = do
    isTheSpectralWatcher <- eid <=~> enemyIs Enemies.theSpectralWatcher
    pure $
      toModifiers
        a
        [ ForcePrey $ Prey $ InvestigatorWithId $ treacheryDrawnBy a
        | isTheSpectralWatcher
        ]
  getModifiersFor _ _ = pure []

instance RunMessage WatchersGrasp where
  runMessage msg t@(WatchersGrasp attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      theSpectralWatcher <- selectJust (enemyIs Enemies.theSpectralWatcher)
      unengaged <- selectNone $ investigatorEngagedWith theSpectralWatcher
      leadInvestigatorId <- getLeadInvestigatorId

      pushAll $
        [ HealDamage (EnemyTarget theSpectralWatcher) (toSource attrs) 3
        , Ready (EnemyTarget theSpectralWatcher)
        ]
          <> ( guard unengaged
                *> [ CheckWindow
                      [leadInvestigatorId]
                      [ Window
                          Timing.When
                          (Window.MovedFromHunter theSpectralWatcher)
                      ]
                   , HunterMove theSpectralWatcher
                   ]
             )
          <> [RevelationChoice iid (toSource attrs) 1]
      pure t
    RevelationChoice _ (isSource attrs -> True) _ -> do
      theSpectralWatcher <- selectJust (enemyIs Enemies.theSpectralWatcher)
      iids <- selectList $ investigatorEngagedWith theSpectralWatcher

      modifiers' <- getModifiers (EnemyTarget theSpectralWatcher)
      unless (CannotAttack `elem` modifiers') $ do
        pushAll $
          map
            ( \iid' ->
                EnemyWillAttack $
                  (enemyAttack theSpectralWatcher attrs iid')
                    { attackExhaustsEnemy = True
                    }
            )
            iids
      pure t
    _ -> WatchersGrasp <$> runMessage msg attrs
