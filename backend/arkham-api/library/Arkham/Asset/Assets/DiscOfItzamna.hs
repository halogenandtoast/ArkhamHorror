module Arkham.Asset.Assets.DiscOfItzamna where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Matcher hiding (EnemyEvaded, NonAttackDamageEffect)
import Arkham.Message.Lifted.Choose
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DiscOfItzamna = DiscOfItzamna AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discOfItzamna :: AssetCard DiscOfItzamna
discOfItzamna = asset DiscOfItzamna Cards.discOfItzamna

instance HasAbilities DiscOfItzamna where
  getAbilities (DiscOfItzamna a) =
    [controlled_ a 1 $ triggered (EnemySpawns #when YourLocation NonEliteEnemy) (discardCost a)]

instance RunMessage DiscOfItzamna where
  runMessage msg a@(DiscOfItzamna attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (map windowType -> [Window.EnemySpawns eid _]) _ -> do
      canDamage <- eid <=~> EnemyCanBeDamagedBySource (attrs.ability 1)
      canEvade <- eid <=~> EnemyCanBeEvadedBy (attrs.ability 1)
      card <- fetchCard eid
      chooseOrRunOneM iid $ withI18n do
        cardNameVar card $ labeledValidate' canEvade "automaticallyEvade" $ automaticallyEvadeEnemy iid eid
        countVar 2
          $ labeledValidate' canDamage "dealDamage"
          $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2 eid
      pure a
    _ -> DiscOfItzamna <$> liftRunMessage msg attrs
