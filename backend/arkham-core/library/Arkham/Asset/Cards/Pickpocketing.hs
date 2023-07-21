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
import Arkham.Timing qualified as Timing

newtype Pickpocketing = Pickpocketing AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pickpocketing :: AssetCard Pickpocketing
pickpocketing = asset Pickpocketing Cards.pickpocketing

instance HasAbilities Pickpocketing where
  getAbilities (Pickpocketing a) =
    [ restrictedAbility a 1 ControlsThis $
        ReactionAbility
          (Matcher.EnemyEvaded Timing.After You AnyEnemy)
          (ExhaustCost $ toTarget a)
    ]

instance RunMessage Pickpocketing where
  runMessage msg a@(Pickpocketing attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      drawing <- drawCards iid attrs 1
      push drawing
      pure a
    _ -> Pickpocketing <$> runMessage msg attrs
