module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.StirringInHisSleep (stirringInHisSleep) where

import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as ItMEnemies
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon qualified as Enemies
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype StirringInHisSleep = StirringInHisSleep TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stirringInHisSleep :: TreacheryCard StirringInHisSleep
stirringInHisSleep = treachery StirringInHisSleep Cards.stirringInHisSleep

-- | Dagon counts as slumbering while his "Deep in Slumber" side is the one in play.
slumberingDagon :: EnemyMatcher
slumberingDagon =
  mapOneOf
    enemyIs
    [Enemies.dagonDeepInSlumber, ItMEnemies.dagonDeepInSlumberIntoTheMaelstrom]

instance RunMessage StirringInHisSleep where
  runMessage msg t@(StirringInHisSleep attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      selectOne slumberingDagon >>= \case
        Nothing -> gainSurge attrs
        Just dagon -> do
          placeDoom attrs dagon 1
          nearby <-
            select
              $ InvestigatorAt
              $ LocationWithDistanceFromAtMost 1 (locationWithEnemy dagon) Anywhere
          for_ nearby \iid -> assignHorror iid attrs 1
      pure t
    _ -> StirringInHisSleep <$> liftRunMessage msg attrs
