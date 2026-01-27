module Arkham.Asset.Assets.LoganHastingsBountyHunter (loganHastingsBountyHunter) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated)
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype LoganHastingsBountyHunter = LoganHastingsBountyHunter AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loganHastingsBountyHunter :: AssetCard LoganHastingsBountyHunter
loganHastingsBountyHunter = ally LoganHastingsBountyHunter Cards.loganHastingsBountyHunter (2, 1)

instance HasModifiersFor LoganHastingsBountyHunter where
  getModifiersFor (LoganHastingsBountyHunter a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities LoganHastingsBountyHunter where
  getAbilities (LoganHastingsBountyHunter a) =
    [ controlled a 1 (youExist can.gain.resources)
        $ triggered (EnemyDefeated #after You ByAny AnyEnemy) (exhaust a)
    ]

instance RunMessage LoganHastingsBountyHunter where
  runMessage msg a@(LoganHastingsBountyHunter attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> LoganHastingsBountyHunter <$> liftRunMessage msg attrs
