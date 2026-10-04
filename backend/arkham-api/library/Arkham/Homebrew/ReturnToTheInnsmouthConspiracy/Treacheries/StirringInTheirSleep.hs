module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.StirringInTheirSleep (
  stirringInTheirSleep,
) where

import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.IntoTheMaelstrom qualified as Enemies
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype StirringInTheirSleep = StirringInTheirSleep TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stirringInTheirSleep :: TreacheryCard StirringInTheirSleep
stirringInTheirSleep = treachery StirringInTheirSleep Cards.stirringInTheirSleep

{- | Hydra takes damage and Dagon horror, each only while slumbering; surge only if
neither was. The Into the Maelstrom printings are the ones that can be in play here.
-}
instance RunMessage StirringInTheirSleep where
  runMessage msg t@(StirringInTheirSleep attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      mHydra <- selectOne $ enemyIs Enemies.hydraDeepInSlumber
      mDagon <- selectOne $ enemyIs Enemies.dagonDeepInSlumberIntoTheMaelstrom
      for_ mHydra \hydra -> do
        placeDoom attrs hydra 1
        nearby <-
          select
            $ InvestigatorAt
            $ LocationWithDistanceFromAtMost 1 (locationWithEnemy hydra) Anywhere
        for_ nearby \iid -> assignDamage iid attrs 1
      for_ mDagon \dagon -> do
        placeDoom attrs dagon 1
        nearby <-
          select
            $ InvestigatorAt
            $ LocationWithDistanceFromAtMost 1 (locationWithEnemy dagon) Anywhere
        for_ nearby \iid -> assignHorror iid attrs 1
      when (isNothing mHydra && isNothing mDagon) $ gainSurge attrs
      pure t
    _ -> StirringInTheirSleep <$> liftRunMessage msg attrs
