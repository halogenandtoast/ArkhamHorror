module Arkham.Asset.Assets.Bodyguard2 (bodyguard2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Bodyguard2 = Bodyguard2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bodyguard2 :: AssetCard Bodyguard2
bodyguard2 = ally Bodyguard2 Cards.bodyguard2 (3, 2)

instance HasAbilities Bodyguard2 where
  getAbilities (Bodyguard2 a) =
    [ controlled a 1 (exists $ EnemyAt YourLocation <> EnemyCanBeDamagedBySource (a.ability 1))
        $ triggered_ (AssetDefeated #when ByAny $ be a)
    ]

instance HasModifiersFor Bodyguard2 where
  getModifiersFor (Bodyguard2 a) = for_ a.controller \iid -> do
    modifySelect
      a
      (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
      [CanAssignDamageToAsset a.id]

instance RunMessage Bodyguard2 where
  runMessage msg a@(Bodyguard2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select $ EnemyAt (locationWithInvestigator iid) <> EnemyCanBeDamagedBySource (toSource attrs)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2
      pure a
    _ -> Bodyguard2 <$> liftRunMessage msg attrs
