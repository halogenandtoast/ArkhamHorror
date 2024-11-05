module Arkham.Treachery.Cards.HideousLullaby (hideousLullaby, HideousLullaby (..)) where

import Arkham.Calculation
import Arkham.Enemy.Types (Field (EnemyFight))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (DeepOne))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HideousLullaby = HideousLullaby TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hideousLullaby :: TreacheryCard HideousLullaby
hideousLullaby = treachery HideousLullaby Cards.hideousLullaby

instance RunMessage HideousLullaby where
  runMessage msg t@(HideousLullaby attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      deepOnes <- selectMaybeMax EnemyFight $ EnemyWithTrait DeepOne
      if null deepOnes
        then gainSurge attrs
        else do
          sid <- getRandom
          chooseTargetM iid deepOnes \deepOne ->
            beginSkillTest sid iid attrs iid #willpower (EnemyMaybeFieldCalculation deepOne EnemyFight)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> HideousLullaby <$> liftRunMessage msg attrs
