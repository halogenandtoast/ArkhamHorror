module Arkham.Asset.Cards.EsotericFormula (esotericFormula, EsotericFormula (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Enemy.Types (Field (..))
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait

newtype EsotericFormula = EsotericFormula AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericFormula :: AssetCard EsotericFormula
esotericFormula = asset EsotericFormula Cards.esotericFormula

instance HasAbilities EsotericFormula where
  getAbilities (EsotericFormula x) =
    [ fightAbility x 1 mempty
        $ ControlsThis
        <> exists (CanFightEnemy (x.ability 1) <> EnemyWithTrait Abomination)
    ]

instance HasModifiersFor EsotericFormula where
  getModifiersFor (InvestigatorTarget iid) (EsotericFormula attrs) | controlledBy attrs iid = do
    toModifiers attrs . toList <$> runMaybeT do
      EnemyTarget eid <- MaybeT getSkillTestTarget
      guardM $ isAbilitySource attrs 1 <$> MaybeT getSkillTestSource
      Action.Fight <- MaybeT getSkillTestAction
      guardM $ (== iid) <$> MaybeT getSkillTestInvestigator
      clueCount <- lift $ field EnemyClues eid
      pure $ SkillModifier #willpower (clueCount * 2)
  getModifiersFor _ _ = pure []

instance RunMessage EsotericFormula where
  runMessage msg a@(EsotericFormula attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <-
        aspect iid source (#willpower `InsteadOf` #combat)
          $ mkChooseFightMatch iid source (EnemyWithTrait Abomination)
      pushAll $ leftOr chooseFight
      pure a
    _ -> EsotericFormula <$> runMessage msg attrs
