module Arkham.Fight (module Arkham.Fight, module Arkham.Fight.Types) where

import Arkham.Classes.HasGame
import Arkham.Fight.Types
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillType
import Arkham.Source

withSkillType :: SkillType -> ChooseFight -> ChooseFight
withSkillType skillType chooseFight = chooseFight {chooseFightSkillType = skillType}

onlyChooseFight :: Functor m => m ChooseFight -> m ChooseFight
onlyChooseFight = fmap $ \chooseFight -> chooseFight {chooseFightOnlyChoose = True}

mkChooseFight :: (Sourceable source, HasGame m) => InvestigatorId -> source -> m ChooseFight
mkChooseFight iid source =
  pure
    $ ChooseFight
      { chooseFightInvestigator = iid
      , chooseFightEnemyMatcher = AnyEnemy
      , chooseFightSource = toSource source
      , chooseFightTarget = Nothing
      , chooseFightSkillType = #combat
      , chooseFightIsAction = False
      , chooseFightOnlyChoose = False
      , chooseFightOverride = False
      }

mkChooseFightMatch
  :: (Sourceable source, HasGame m) => InvestigatorId -> source -> EnemyMatcher -> m ChooseFight
mkChooseFightMatch iid source matcher = do
  chooseFight <- mkChooseFight iid source
  let
    isOverriden = case matcher of
      CanFightEnemyWithOverride {} -> True
      _ -> False

  pure $ chooseFight {chooseFightEnemyMatcher = matcher, chooseFightOverride = isOverriden}
