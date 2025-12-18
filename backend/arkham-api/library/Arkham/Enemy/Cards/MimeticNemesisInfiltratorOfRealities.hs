module Arkham.Enemy.Cards.MimeticNemesisInfiltratorOfRealities (mimeticNemesisInfiltratorOfRealities) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)

newtype MimeticNemesisInfiltratorOfRealities = MimeticNemesisInfiltratorOfRealities EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mimeticNemesisInfiltratorOfRealities :: EnemyCard MimeticNemesisInfiltratorOfRealities
mimeticNemesisInfiltratorOfRealities =
  enemy
    MimeticNemesisInfiltratorOfRealities
    Cards.mimeticNemesisInfiltratorOfRealities
    (5, Static 3, 5)
    (3, 3)

instance HasModifiersFor MimeticNemesisInfiltratorOfRealities where
  getModifiersFor (MimeticNemesisInfiltratorOfRealities a) = do
    n <- perPlayer 3
    modifySelf a [HealthModifier n]

instance RunMessage MimeticNemesisInfiltratorOfRealities where
  runMessage msg (MimeticNemesisInfiltratorOfRealities attrs) = runQueueT $ case msg of
    _ -> MimeticNemesisInfiltratorOfRealities <$> liftRunMessage msg attrs
