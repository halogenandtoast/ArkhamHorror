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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericFormula :: AssetCard EsotericFormula
esotericFormula = asset EsotericFormula Cards.esotericFormula

instance HasAbilities EsotericFormula where
  getAbilities (EsotericFormula x) =
    [ restrictedAbility
        x
        1
        ( ControlsThis
            <> EnemyCriteria
              (EnemyExists $ CanFightEnemy (toAbilitySource x 1) <> EnemyWithTrait Abomination)
        )
        (ActionAbility (Just Action.Fight) (ActionCost 1))
    ]

instance HasModifiersFor EsotericFormula where
  getModifiersFor (InvestigatorTarget iid) (EsotericFormula attrs)
    | controlledBy attrs iid = do
        mSkillTestTarget <- getSkillTestTarget
        mSkillTestSource <- getSkillTestSource
        case (mSkillTestSource, mSkillTestTarget) of
          (Just (SkillTestSource iid' _ source (Just Action.Fight)), Just (EnemyTarget eid))
            | isSource attrs source && iid == iid' ->
                do
                  clueCount <- field EnemyClues eid
                  pure $
                    toModifiers
                      attrs
                      [SkillModifier SkillWillpower (clueCount * 2)]
          _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage EsotericFormula where
  runMessage msg a@(EsotericFormula attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ push
              ( ChooseFightEnemy
                  iid
                  source
                  Nothing
                  SkillWillpower
                  (EnemyWithTrait Abomination)
                  False
              )
    _ -> EsotericFormula <$> runMessage msg attrs
