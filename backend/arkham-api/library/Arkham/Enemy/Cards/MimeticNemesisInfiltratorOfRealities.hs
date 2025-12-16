module Arkham.Enemy.Cards.MimeticNemesisInfiltratorOfRealities (mimeticNemesisInfiltratorOfRealities) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MimeticNemesisInfiltratorOfRealities = MimeticNemesisInfiltratorOfRealities EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mimeticNemesisInfiltratorOfRealities :: EnemyCard MimeticNemesisInfiltratorOfRealities
mimeticNemesisInfiltratorOfRealities = enemy MimeticNemesisInfiltratorOfRealities Cards.mimeticNemesisInfiltratorOfRealities (0, Static 1, 0) (0, 0)

instance RunMessage MimeticNemesisInfiltratorOfRealities where
  runMessage msg (MimeticNemesisInfiltratorOfRealities attrs) = runQueueT $ case msg of
    _ -> MimeticNemesisInfiltratorOfRealities <$> liftRunMessage msg attrs
