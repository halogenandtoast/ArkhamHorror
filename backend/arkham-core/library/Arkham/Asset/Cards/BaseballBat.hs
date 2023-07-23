module Arkham.Asset.Cards.BaseballBat (
  BaseballBat (..),
  baseballBat,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType

newtype BaseballBat = BaseballBat AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseballBat :: AssetCard BaseballBat
baseballBat = asset BaseballBat Cards.baseballBat

instance HasModifiersFor BaseballBat where
  getModifiersFor (InvestigatorTarget iid) (BaseballBat a) | controlledBy a iid = do
    mAction <- getSkillTestAction
    mSource <- getSkillTestSource
    case (mAction, mSource) of
      (Just Action.Fight, Just source)
        | isSource a source -> pure $ toModifiers a [DamageDealt 1]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities BaseballBat where
  getAbilities (BaseballBat a) = [fightAbility a 1 (ActionCost 1) ControlsThis]

instance RunMessage BaseballBat where
  runMessage msg a@(BaseballBat attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      pushAll
        [ skillTestModifier attrs iid (SkillModifier SkillCombat 2)
        , CreateEffect "01074" Nothing source (toTarget iid)
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
      pure a
    _ -> BaseballBat <$> runMessage msg attrs
