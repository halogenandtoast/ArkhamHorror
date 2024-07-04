module Arkham.Asset.Cards.EsotericFormula (esotericFormula, EsotericFormula (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Fight
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait

newtype EsotericFormula = EsotericFormula AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericFormula :: AssetCard EsotericFormula
esotericFormula = asset EsotericFormula Cards.esotericFormula

instance HasAbilities EsotericFormula where
  getAbilities (EsotericFormula x) =
    [ fightAbility x 1 mempty
        $ ControlsThis
        <> exists (CanFightEnemy (x.ability 1) <> EnemyWithTrait Abomination)
    ]

instance RunMessage EsotericFormula where
  runMessage msg a@(EsotericFormula attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      skillTestModifier source iid
        $ ForEach (EnemyTargetFieldCalculation EnemyClues) [SkillModifier #willpower 2]
      chooseFight <-
        aspect iid source (#willpower `InsteadOf` #combat)
          $ mkChooseFightMatch iid source (EnemyWithTrait Abomination)
      pushAll $ leftOr chooseFight
      pure a
    _ -> EsotericFormula <$> liftRunMessage msg attrs
