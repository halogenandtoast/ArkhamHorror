module Arkham.Asset.Assets.SpectralWeb (spectralWeb) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Trait (Trait (Geist))

newtype SpectralWeb = SpectralWeb AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralWeb :: AssetCard SpectralWeb
spectralWeb = asset SpectralWeb Cards.spectralWeb

instance HasAbilities SpectralWeb where
  getAbilities (SpectralWeb attrs) =
    [ controlled attrs 1 (exists $ CanFightEnemy (toSource attrs) <> EnemyWithTrait Geist)
        $ fightAction
        $ GroupClueCostRange (1, 3) YourLocation
    ]

instance RunMessage SpectralWeb where
  runMessage msg a@(SpectralWeb attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalCluePayment -> x) -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseOneM iid $ for [#willpower, #combat] \sType -> do
        skillLabeled sType do
          skillTestModifiers sid source iid [AnySkillValue x, DamageDealt x]
          chooseFightEnemyEdit sid iid source (withSkillType sType)
      pure a
    _ -> SpectralWeb <$> liftRunMessage msg attrs
