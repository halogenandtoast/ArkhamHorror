module Arkham.Types.Asset.Cards.EsotericFormula
  ( esotericFormula
  , EsotericFormula(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype EsotericFormula = EsotericFormula AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericFormula :: AssetCard EsotericFormula
esotericFormula = asset EsotericFormula Cards.esotericFormula

instance HasAbilities EsotericFormula where
  getAbilities (EsotericFormula x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> EnemyCriteria
          (EnemyExists $ CanFightEnemy <> EnemyWithTrait Abomination)
        )
        (ActionAbility (Just Action.Fight) (ActionCost 1))
    ]

instance HasCount ClueCount env EnemyId => HasModifiersFor env EsotericFormula where
  getModifiersFor (SkillTestSource iid' _ source (EnemyTarget eid) (Just Action.Fight)) (InvestigatorTarget iid) (EsotericFormula attrs)
    | ownedBy attrs iid && isSource attrs source && iid' == iid
    = do
      clueCount <- unClueCount <$> getCount eid
      pure $ toModifiers attrs [SkillModifier SkillWillpower (clueCount * 2)]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env EsotericFormula where
  runMessage msg a@(EsotericFormula attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (ChooseFightEnemy
        iid
        source
        Nothing
        SkillWillpower
        (EnemyWithTrait Abomination)
        False
      )
    _ -> EsotericFormula <$> runMessage msg attrs
