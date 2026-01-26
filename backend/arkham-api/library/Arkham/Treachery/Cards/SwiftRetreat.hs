module Arkham.Treachery.Cards.SwiftRetreat (swiftRetreat) where

import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.SkillTest.Lifted (beginSkillTestEdit, getSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.SkillTest.Base (setIsRevelation)
import Arkham.Trait (Trait (Coterie))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SwiftRetreat = SwiftRetreat TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swiftRetreat :: TreacheryCard SwiftRetreat
swiftRetreat = treachery SwiftRetreat Cards.swiftRetreat

instance RunMessage SwiftRetreat where
  runMessage msg t@(SwiftRetreat attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      coterie <- select $ NearestEnemyToFallback iid $ EnemyWithTrait Coterie
      if null coterie
        then gainSurge attrs
        else do
          sid <- getRandom
          chooseTargetM iid coterie \e -> beginSkillTestEdit sid iid attrs e #agility (Fixed 3) setIsRevelation
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      whenJustM getSkillTestTargetedEnemy \e -> do
        initiateEnemyAttack e (attrs.ability 1) iid
        whenM (matches e (EnemyWithConcealed <> not_ (EnemyWithPlacement InTheShadows))) do
          place e InTheShadows
          resolveConcealed iid e
      pure t
    _ -> SwiftRetreat <$> liftRunMessage msg attrs
