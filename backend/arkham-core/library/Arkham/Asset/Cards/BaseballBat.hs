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
  getModifiersFor (InvestigatorTarget iid) (BaseballBat a)
    | controlledBy a iid = do
        mSkillTestSource <- getSkillTestSource
        case mSkillTestSource of
          Just (SkillTestSource _ _ source (Just Action.Fight))
            | isSource a source -> pure $ toModifiers a [DamageDealt 1]
          _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities BaseballBat where
  getAbilities (BaseballBat a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility (Just Action.Fight) (ActionCost 1)
    ]

instance RunMessage BaseballBat where
  runMessage msg a@(BaseballBat attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ pushAll
              [ skillTestModifier
                  attrs
                  (InvestigatorTarget iid)
                  (SkillModifier SkillCombat 2)
              , CreateEffect "01074" Nothing source (InvestigatorTarget iid)
              , ChooseFightEnemy iid source Nothing SkillCombat mempty False
              ]
    _ -> BaseballBat <$> runMessage msg attrs
