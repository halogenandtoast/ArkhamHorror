module Arkham.Treachery.Cards.TheEndIsNigh (theEndIsNigh) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheEndIsNigh = TheEndIsNigh TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEndIsNigh :: TreacheryCard TheEndIsNigh
theEndIsNigh = treachery TheEndIsNigh Cards.theEndIsNigh

instance RunMessage TheEndIsNigh where
  runMessage msg t@(TheEndIsNigh attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 1, CurrentAgendaStepCalculation (Fixed 4)]
      pure t
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      cultists <- select $ EnemyWithTrait Cultist
      if null cultists
        then placeDoom attrs azathoth 1
        else do
          doom <- getSum <$> foldMapM (fieldMap EnemyDoom Sum) cultists
          for_ cultists $ removeAllDoom attrs
          placeDoom attrs azathoth doom
      pure t
    _ -> TheEndIsNigh <$> liftRunMessage msg attrs
