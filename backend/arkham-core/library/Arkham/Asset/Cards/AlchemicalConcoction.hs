module Arkham.Asset.Cards.AlchemicalConcoction (
  alchemicalConcoction,
  AlchemicalConcoction (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype AlchemicalConcoction = AlchemicalConcoction AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

alchemicalConcoction :: AssetCard AlchemicalConcoction
alchemicalConcoction = asset AlchemicalConcoction Cards.alchemicalConcoction

instance HasAbilities AlchemicalConcoction where
  getAbilities (AlchemicalConcoction a) = [fightAbility a 1 mempty ControlsThis]

instance HasModifiersFor AlchemicalConcoction where
  getModifiersFor (InvestigatorTarget _) (AlchemicalConcoction a) = do
    mAction <- getSkillTestAction
    mSource <- getSkillTestSource
    mTarget <- getSkillTestTarget
    case (mAction, mSource, mTarget) of
      (Just Action.Fight, Just source, Just (EnemyTarget eid)) | isSource a source -> do
        isTheExperiement <- enemyMatches eid $ EnemyWithTitle "The Experiment"
        pure $ toModifiers a [DamageDealt 6 | isTheExperiement]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage AlchemicalConcoction where
  runMessage msg a@(AlchemicalConcoction attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifier attrs iid (DamageDealt 1)
        , CreateEffect "01060" Nothing (toAbilitySource attrs 1) (toTarget iid)
        , ChooseFightEnemy iid (toAbilitySource attrs 1) Nothing SkillWillpower mempty False
        ]
      pure a
    _ -> AlchemicalConcoction <$> runMessage msg attrs
