module Arkham.Asset.Cards.BaseballBat (
  BaseballBat (..),
  baseballBat,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

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
      (Just Action.Fight, Just (isSource a -> True)) -> pure $ toModifiers a [DamageDealt 1]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities BaseballBat where
  getAbilities (BaseballBat a) = [fightAbility a 1 (ActionCost 1) ControlsThis]

instance RunMessage BaseballBat where
  runMessage msg a@(BaseballBat attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifier attrs iid (SkillModifier #combat 2)
        , CreateEffect "01074" Nothing (toAbilitySource attrs 1) (toTarget iid)
        , ChooseFightEnemy iid (toAbilitySource attrs 1) Nothing #combat mempty False
        ]
      pure a
    _ -> BaseballBat <$> runMessage msg attrs
