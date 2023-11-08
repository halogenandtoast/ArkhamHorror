module Arkham.Asset.Cards.Lupara3 (
  lupara3,
  Lupara3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Metadata = Metadata {justPlayed :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Lupara3 = Lupara3 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lupara3 :: AssetCard Lupara3
lupara3 = asset (Lupara3 . (`with` Metadata True)) Cards.lupara3

instance HasAbilities Lupara3 where
  getAbilities (Lupara3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility ([Action.Fight])
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage Lupara3 where
  runMessage msg a@(Lupara3 (attrs `With` metadata)) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      let n = if justPlayed metadata then 2 else 1
      a
        <$ pushAll
          [ skillTestModifiers
              attrs
              (InvestigatorTarget iid)
              [DamageDealt n, SkillModifier SkillCombat n]
          , ChooseFightEnemy iid source Nothing SkillCombat mempty False
          ]
    EndTurn _ -> pure . Lupara3 $ attrs `with` Metadata False
    _ -> Lupara3 . (`with` metadata) <$> runMessage msg attrs
