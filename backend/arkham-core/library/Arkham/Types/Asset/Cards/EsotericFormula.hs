module Arkham.Types.Asset.Cards.EsotericFormula
  ( esotericFormula
  , EsotericFormula(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Control.Monad.Extra

newtype EsotericFormula = EsotericFormula AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericFormula :: AssetCard EsotericFormula
esotericFormula = asset EsotericFormula Cards.esotericFormula

instance (HasActions env ActionType, HasSet Trait env EnemyId, HasSet FightableEnemyId env (InvestigatorId, Source)) => HasActions env EsotericFormula where
  getActions iid window (EsotericFormula attrs) | ownedBy attrs iid =
    withBaseActions iid window attrs $ do
      fightAvailable <- hasFightActions iid window
      fightableEnemies <- map unFightableEnemyId
        <$> getSetList (iid, toSource attrs)
      anyAbominations <- anyM
        (fmap (member Abomination) . getSet @Trait)
        fightableEnemies
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource attrs)
              1
              (ActionAbility (Just Action.Fight) (ActionCost 1))
            )
        | fightAvailable && anyAbominations
        ]
  getActions iid window (EsotericFormula attrs) = getActions iid window attrs

instance HasCount ClueCount env EnemyId => HasModifiersFor env EsotericFormula where
  getModifiersFor (SkillTestSource iid' _ source (EnemyTarget eid) (Just Action.Fight)) (InvestigatorTarget iid) (EsotericFormula attrs)
    | ownedBy attrs iid && isSource attrs source && iid' == iid
    = do
      clueCount <- unClueCount <$> getCount eid
      pure $ toModifiers attrs [SkillModifier SkillWillpower (clueCount * 2)]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env EsotericFormula where
  runMessage msg a@(EsotericFormula attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (ChooseFightEnemy
          iid
          source
          SkillWillpower
          (singleton Abomination)
          False
        )
    _ -> EsotericFormula <$> runMessage msg attrs
