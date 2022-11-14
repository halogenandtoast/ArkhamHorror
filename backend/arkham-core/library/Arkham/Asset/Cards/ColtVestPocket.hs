module Arkham.Asset.Cards.ColtVestPocket
  ( coltVestPocket
  , ColtVestPocket(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype ColtVestPocket = ColtVestPocket AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coltVestPocket :: AssetCard ColtVestPocket
coltVestPocket = asset ColtVestPocket Cards.coltVestPocket

instance HasAbilities ColtVestPocket where
  getAbilities (ColtVestPocket a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Ammo 1
    , restrictedAbility a 2 ControlsThis $ ForcedAbility $ RoundEnds Timing.When
    ]

instance RunMessage ColtVestPocket where
  runMessage msg a@(ColtVestPocket attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers
          (toSource attrs)
          (InvestigatorTarget iid)
          [SkillModifier SkillCombat 1, DamageDealt 1]
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ Discard (toTarget attrs)
      pure a
    _ -> ColtVestPocket <$> runMessage msg attrs
