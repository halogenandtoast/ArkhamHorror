module Arkham.Asset.Assets.RookieCop (rookieCop) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher

newtype RookieCop = RookieCop AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rookieCop :: AssetCard RookieCop
rookieCop = ally RookieCop Cards.rookieCop (2, 2)

instance HasModifiersFor RookieCop where
  getModifiersFor (RookieCop a) = for_ a.controller \iid -> do
    modifySelect
      a
      (not_ (InvestigatorWithId iid) <> at_ (locationWithAsset a))
      [CanAssignDamageToAsset a.id, CanAssignHorrorToAsset a.id]

instance HasAbilities RookieCop where
  getAbilities (RookieCop a) = [controlled_ a 1 $ freeReaction (AssetDefeated #when ByAny $ be a)]

instance RunMessage RookieCop where
  runMessage msg a@(RookieCop attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure a
    _ -> RookieCop <$> liftRunMessage msg attrs
