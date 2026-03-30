module Arkham.Asset.Assets.PrivatePractice (privatePractice) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype PrivatePractice = PrivatePractice AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

privatePractice :: AssetCard PrivatePractice
privatePractice = asset PrivatePractice Cards.privatePractice

instance HasAbilities PrivatePractice where
  getAbilities (PrivatePractice a) =
    [ controlled_ a 1
        $ triggered (InvestigatorHealed #after #horror Anyone (SourceOwnedBy You)) (exhaust a)
    ]

instance RunMessage PrivatePractice where
  runMessage msg a@(PrivatePractice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> PrivatePractice <$> liftRunMessage msg attrs
