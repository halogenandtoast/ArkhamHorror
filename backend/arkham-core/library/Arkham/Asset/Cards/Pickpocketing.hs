module Arkham.Asset.Cards.Pickpocketing (
  Pickpocketing (..),
  pickpocketing,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype Pickpocketing = Pickpocketing AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pickpocketing :: AssetCard Pickpocketing
pickpocketing = asset Pickpocketing Cards.pickpocketing

instance HasAbilities Pickpocketing where
  getAbilities (Pickpocketing a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (Matcher.EnemyEvaded #after You AnyEnemy) (exhaust a)
    ]

instance RunMessage Pickpocketing where
  runMessage msg a@(Pickpocketing attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> Pickpocketing <$> runMessage msg attrs
