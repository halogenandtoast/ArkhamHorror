module Arkham.Asset.Cards.HealingWords (
  healingWords,
  HealingWords (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype HealingWords = HealingWords AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

healingWords :: AssetCard HealingWords
healingWords = asset HealingWords Cards.healingWords

instance HasAbilities HealingWords where
  getAbilities (HealingWords a) =
    [ restrictedAbility
        a
        1
        ( ControlsThis
            <> exists (HealableInvestigator (toAbilitySource a 1) #damage $ InvestigatorAt YourLocation)
        )
        $ actionAbilityWithCost
        $ assetUseCost a Charge 1
    ]

instance RunMessage HealingWords where
  runMessage msg a@(HealingWords attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ HealDamage (toTarget iid) (toSource attrs) 1
      pure a
    _ -> HealingWords <$> runMessage msg attrs
