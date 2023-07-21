module Arkham.Asset.Cards.ColtVestPocket2 (
  coltVestPocket2,
  ColtVestPocket2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype Metadata = Metadata {abilityTriggered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ColtVestPocket2 = ColtVestPocket2 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coltVestPocket2 :: AssetCard ColtVestPocket2
coltVestPocket2 =
  asset (ColtVestPocket2 . (`with` Metadata False)) Cards.coltVestPocket2

instance HasAbilities ColtVestPocket2 where
  getAbilities (ColtVestPocket2 (a `With` meta)) =
    restrictedAbility
      a
      1
      ControlsThis
      ( ActionAbility (Just Action.Fight) $
          ActionCost 1
            <> UseCost
              (AssetWithId $ toId a)
              Ammo
              1
      )
      : [ restrictedAbility a 2 ControlsThis $
          ForcedAbility $
            RoundEnds
              Timing.When
        | abilityTriggered meta
        ]

instance RunMessage ColtVestPocket2 where
  runMessage msg a@(ColtVestPocket2 (attrs `With` meta)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers
            (toSource attrs)
            (InvestigatorTarget iid)
            [SkillModifier SkillCombat 2, DamageDealt 1]
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure . ColtVestPocket2 $ attrs `with` Metadata True
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ Discard (toAbilitySource attrs 2) (toTarget attrs)
      pure a
    _ -> ColtVestPocket2 . (`with` meta) <$> runMessage msg attrs
