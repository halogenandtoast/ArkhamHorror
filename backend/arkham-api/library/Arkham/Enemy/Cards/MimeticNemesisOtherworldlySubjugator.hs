module Arkham.Enemy.Cards.MimeticNemesisOtherworldlySubjugator (mimeticNemesisOtherworldlySubjugator) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MimeticNemesisOtherworldlySubjugator = MimeticNemesisOtherworldlySubjugator EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mimeticNemesisOtherworldlySubjugator :: EnemyCard MimeticNemesisOtherworldlySubjugator
mimeticNemesisOtherworldlySubjugator = enemy MimeticNemesisOtherworldlySubjugator Cards.mimeticNemesisOtherworldlySubjugator (3, Static 1, 4) (1, 1)

instance RunMessage MimeticNemesisOtherworldlySubjugator where
  runMessage msg (MimeticNemesisOtherworldlySubjugator attrs) = runQueueT $ case msg of
    _ -> MimeticNemesisOtherworldlySubjugator <$> liftRunMessage msg attrs
