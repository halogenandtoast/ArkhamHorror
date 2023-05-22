module Arkham.Asset.Cards.AlchemicalConcoction
  ( alchemicalConcoction
  , AlchemicalConcoction(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype AlchemicalConcoction = AlchemicalConcoction AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalConcoction :: AssetCard AlchemicalConcoction
alchemicalConcoction = asset AlchemicalConcoction Cards.alchemicalConcoction

instance HasAbilities AlchemicalConcoction where
  getAbilities (AlchemicalConcoction a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
    ]

instance HasModifiersFor AlchemicalConcoction where
  getModifiersFor (InvestigatorTarget _) (AlchemicalConcoction a) = do
    mSkillTestSource <- getSkillTestSource
    mSkillTestTarget <- getSkillTestTarget
    case (mSkillTestSource, mSkillTestTarget) of
      (Just (SkillTestSource _ _ source (Just Action.Fight)), Just (EnemyTarget eid))
        | isSource a source
        -> do
          isTheExperiement <- enemyMatches eid $ EnemyWithTitle "The Experiment"
          pure $ toModifiers a [ DamageDealt 6 | isTheExperiement ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage AlchemicalConcoction where
  runMessage msg a@(AlchemicalConcoction attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      a <$ pushAll
        [ skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
        , CreateEffect "01060" Nothing source (InvestigatorTarget iid)
        , ChooseFightEnemy iid source Nothing SkillWillpower mempty False
        ]
    _ -> AlchemicalConcoction <$> runMessage msg attrs
