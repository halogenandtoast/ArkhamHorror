module Arkham.Asset.Cards.DevilFriendOrFoe2 (
  devilFriendOrFoe2,
  DevilFriendOrFoe2 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Matcher
import Arkham.Token

newtype DevilFriendOrFoe2 = DevilFriendOrFoe2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilFriendOrFoe2 :: AssetCard DevilFriendOrFoe2
devilFriendOrFoe2 = allyWith DevilFriendOrFoe2 Cards.devilFriendOrFoe2 (3, 0) (sanityL .~ Nothing)

instance HasAbilities DevilFriendOrFoe2 where
  getAbilities (DevilFriendOrFoe2 a) =
    [ controlledAbility a 1 (youExist InvestigatorWithAnyDamage) $ forced $ TurnBegins #when You
    , controlledAbility a 2 (youExist $ InvestigatorAt Anywhere)
        $ forced
        $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage DevilFriendOrFoe2 where
  runMessage msg a@(DevilFriendOrFoe2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTokens (attrs.ability 1) iid attrs Damage 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (attrs.ability 2)
      for_ enemies $ nonAttackEnemyDamage (attrs.ability 2) 2

      investigators <- select $ colocatedWith iid
      for_ investigators \iid' -> do
        assignDamage iid' (attrs.ability 2) 2

      pure a
    _ -> DevilFriendOrFoe2 <$> liftRunMessage msg attrs
