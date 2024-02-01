module Arkham.Asset.Cards.EsotericFormula (
  esotericFormula,
  EsotericFormula (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Trait

newtype EsotericFormula = EsotericFormula AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

esotericFormula :: AssetCard EsotericFormula
esotericFormula = asset EsotericFormula Cards.esotericFormula

instance HasAbilities EsotericFormula where
  getAbilities (EsotericFormula x) =
    [ fightAbility x 1 mempty
        $ ControlsThis
        <> enemyExists (CanFightEnemy (toAbilitySource x 1) <> EnemyWithTrait Abomination)
    ]

instance HasModifiersFor EsotericFormula where
  getModifiersFor (InvestigatorTarget iid) (EsotericFormula attrs) | controlledBy attrs iid = do
    mTarget <- getSkillTestTarget
    mSource <- getSkillTestSource
    mAction <- getSkillTestAction
    miid <- getSkillTestInvestigator
    case (mAction, mSource, mTarget, miid) of
      (Just Action.Fight, Just source, Just (EnemyTarget eid), Just iid')
        | isSource attrs source && iid == iid' -> do
            clueCount <- field EnemyClues eid
            pure $ toModifiers attrs [SkillModifier SkillWillpower (clueCount * 2)]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage EsotericFormula where
  runMessage msg a@(EsotericFormula attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push
        $ ChooseFightEnemy
          iid
          source
          Nothing
          SkillWillpower
          (EnemyWithTrait Abomination)
          False
      pure a
    _ -> EsotericFormula <$> runMessage msg attrs
