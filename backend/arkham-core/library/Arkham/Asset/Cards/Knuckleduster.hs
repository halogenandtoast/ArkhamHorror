module Arkham.Asset.Cards.Knuckleduster (
  knuckleduster,
  Knuckleduster (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Keyword qualified as Keyword
import Arkham.SkillType

newtype Knuckleduster = Knuckleduster AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knuckleduster :: AssetCard Knuckleduster
knuckleduster = asset Knuckleduster Cards.knuckleduster

instance HasAbilities Knuckleduster where
  getAbilities (Knuckleduster a) = [fightAbility a 1 mempty ControlsThis]

instance HasModifiersFor Knuckleduster where
  getModifiersFor (EnemyTarget eid) (Knuckleduster attrs) = do
    mTarget <- getSkillTestTarget
    mSource <- getSkillTestSource
    mAction <- getSkillTestAction
    case (mAction, mSource, mTarget) of
      (Just Action.Fight, Just source, Just (EnemyTarget eid'))
        | eid == eid' && isSource attrs source ->
            pure $ toModifiers attrs [AddKeyword Keyword.Retaliate]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage Knuckleduster where
  runMessage msg a@(Knuckleduster attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ skillTestModifier attrs iid (DamageDealt 1)
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
      pure a
    _ -> Knuckleduster <$> runMessage msg attrs
