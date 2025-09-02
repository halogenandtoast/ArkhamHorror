module Arkham.Treachery.Cards.VampiresKiss (vampiresKiss) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VampiresKiss = VampiresKiss TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vampiresKiss :: TreacheryCard VampiresKiss
vampiresKiss = treachery VampiresKiss Cards.vampiresKiss

instance RunMessage VampiresKiss where
  runMessage msg t@(VampiresKiss attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ IfInvestigatorExistsCalculation iid (InvestigatorWithDamage $ atMost 3) (Fixed 5) (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignDamage iid (attrs.ability 1) 1
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      enemies <- select $ NearestEnemyTo iid EnemyWithAnyDamage
      if length enemies == 1
        then chooseTargetM iid enemies $ healDamageOn attrs n
        else do
          chooseTargetM iid enemies $ healDamageOn attrs n
          doStep (n - 1) msg'
      pure t
    _ -> VampiresKiss <$> liftRunMessage msg attrs
