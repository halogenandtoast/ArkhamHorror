module Arkham.Enemy.Cards.AlikiZoniUperetriaSpeaksInDeath (alikiZoniUperetriaSpeaksInDeath) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Strategy

newtype AlikiZoniUperetriaSpeaksInDeath = AlikiZoniUperetriaSpeaksInDeath EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alikiZoniUperetriaSpeaksInDeath :: EnemyCard AlikiZoniUperetriaSpeaksInDeath
alikiZoniUperetriaSpeaksInDeath =
  enemy AlikiZoniUperetriaSpeaksInDeath Cards.alikiZoniUperetriaSpeaksInDeath (2, Static 3, 2) (1, 1)

instance HasAbilities AlikiZoniUperetriaSpeaksInDeath where
  getAbilities (AlikiZoniUperetriaSpeaksInDeath a) = extend1 a $ mkAbility a 1 $ forced $ PhaseEnds #when #enemy

instance RunMessage AlikiZoniUperetriaSpeaksInDeath where
  runMessage msg e@(AlikiZoniUperetriaSpeaksInDeath attrs) = runQueueT $ case msg of
    InvestigatorDrawEnemy _ eid | eid == attrs.id -> do
      keysFor attrs >>= traverse_ (`createScarletKeyAt_` AttachedToEnemy attrs.id)
      AlikiZoniUperetriaSpeaksInDeath <$> liftRunMessage msg attrs
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> do
        lookAt
          iid
          (attrs.ability 1)
          iid
          [(FromTopOfDeck 1, DoNothing)]
          (basic NonWeakness)
          (defer attrs IsNotDraw)
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      for_ cards \card -> do
        hollow iid card
        whenMatch card CardWithHollowedCopy do
          skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
          chooseOneAtATimeM iid $ targets skeys shift
      pure e
    _ -> AlikiZoniUperetriaSpeaksInDeath <$> liftRunMessage msg attrs
