module Arkham.Asset.Assets.GrislyMaskIsItActuallyAlive (grislyMask) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype GrislyMaskIsItActuallyAlive = GrislyMaskIsItActuallyAlive AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyMask :: AssetCard GrislyMaskIsItActuallyAlive
grislyMask = asset GrislyMaskIsItActuallyAlive Cards.grislyMask

instance HasAbilities GrislyMaskIsItActuallyAlive where
  getAbilities (GrislyMaskIsItActuallyAlive x) =
    [ controlled_ x 1 $ FastAbility (exhaust x)
    , controlled
        x
        2
        (exists $ at_ YourLocation <> EnemyCanBeDamagedBySource (x.ability 2))
        $ FastAbility (DirectHorrorCost (x.ability 2) You 1 <> exhaust x)
    ]

instance RunMessage GrislyMaskIsItActuallyAlive where
  runMessage msg a@(GrislyMaskIsItActuallyAlive attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyEngagedWith iid
      for_ enemies $ disengageEnemy iid
      for_ enemies enemyCheckEngagement
      nextTurnModifiers
        iid
        (attrs.ability 1)
        iid
        [CannotMove, CannotDealDamage, CannotEngage iid]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <-
        select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (attrs.ability 2)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 2) 2
      pure a
    _ -> GrislyMaskIsItActuallyAlive <$> liftRunMessage msg attrs
