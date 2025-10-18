module Arkham.Asset.Assets.DevilFriendOrFoe2 (devilFriendOrFoe2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token

newtype DevilFriendOrFoe2 = DevilFriendOrFoe2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilFriendOrFoe2 :: AssetCard DevilFriendOrFoe2
devilFriendOrFoe2 = allyWith DevilFriendOrFoe2 Cards.devilFriendOrFoe2 (3, 0) (sanityL .~ Nothing)

instance HasAbilities DevilFriendOrFoe2 where
  getAbilities (DevilFriendOrFoe2 a) =
    [ controlled a 1 (youExist InvestigatorWithAnyDamage) $ forced $ TurnBegins #when You
    , controlled a 2 (youExist $ InvestigatorAt Anywhere) $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage DevilFriendOrFoe2 where
  runMessage msg a@(DevilFriendOrFoe2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTokens (attrs.ability 1) iid attrs Damage 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (attrs.ability 2)
      for_ enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 2) 2
      getConcealed (ForExpose $ toSource iid) iid >>= traverse_ \concealed -> do
        chooseOneM iid do
          labeled "Damage concealed card" $ doFlip iid (attrs.ability 2) concealed
          labeled "Do not damage concealed card" nothing

      investigators <- select $ colocatedWith iid
      for_ investigators \iid' -> do
        assignDamage iid' (attrs.ability 2) 2

      pure a
    _ -> DevilFriendOrFoe2 <$> liftRunMessage msg attrs
