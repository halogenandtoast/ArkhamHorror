module Arkham.Asset.Assets.Bodyguard (bodyguard) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Bodyguard = Bodyguard AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bodyguard :: AssetCard Bodyguard
bodyguard = ally Bodyguard Cards.bodyguard (2, 1)

instance HasAbilities Bodyguard where
  getAbilities (Bodyguard a) =
    [ controlled a 1 (exists $ EnemyAt YourLocation <> EnemyCanBeDamagedBySource (a.ability 1))
        $ triggered_ (AssetDefeated #when ByAny $ be a)
    ]

instance HasModifiersFor Bodyguard where
  getModifiersFor (Bodyguard a) = for_ a.controller \iid -> do
    modifySelect
      a
      (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
      [CanAssignDamageToAsset a.id]

instance RunMessage Bodyguard where
  runMessage msg a@(Bodyguard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <-
        select $ EnemyAt (locationWithInvestigator iid) <> EnemyCanBeDamagedBySource (toSource attrs)
      chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
      pure a
    _ -> Bodyguard <$> liftRunMessage msg attrs
