module Arkham.Enemy.Cards.JeromeDavids (jeromeDavids) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher

newtype JeromeDavids = JeromeDavids EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeromeDavids :: EnemyCard JeromeDavids
jeromeDavids = enemy JeromeDavids Cards.jeromeDavids (4, Static 4, 4) (1, 1)

instance HasAbilities JeromeDavids where
  getAbilities (JeromeDavids a) =
    extend1 a
      $ restricted a 1 (youExist InvestigatorWithDiscardableCard)
      $ forced
      $ EnemyEngaged #after You (be a)

instance RunMessage JeromeDavids where
  runMessage msg e@(JeromeDavids attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardFromHand iid attrs DiscardChoose 2
      pure e
    _ -> JeromeDavids <$> liftRunMessage msg attrs
