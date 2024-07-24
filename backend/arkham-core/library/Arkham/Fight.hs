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

withFightOverride :: ChooseFight -> ChooseFight
withFightOverride chooseFight = chooseFight {chooseFightOverride = True}

mkChooseFight
  :: (Sourceable source, HasGame m) => SkillTestId -> InvestigatorId -> source -> m ChooseFight
mkChooseFight sid iid source =
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
      , chooseFightSkillTest = sid
      }

mkChooseFightMatch
  :: (Sourceable source, HasGame m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> EnemyMatcher
  -> m ChooseFight
mkChooseFightMatch sid iid source matcher = do
  chooseFight <- mkChooseFight sid iid source
  let
    isOverriden = case matcher of
      CanFightEnemyWithOverride {} -> True
      _ -> False

  pure $ chooseFight {chooseFightEnemyMatcher = matcher, chooseFightOverride = isOverriden}

mkFightEnemy
  :: (Sourceable source, HasGame m, AsId e, IdOf e ~ EnemyId)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> e
  -> m ChooseFight
mkFightEnemy sid iid source enemy = do
  mkChooseFight sid iid source <&> \cf -> cf {chooseFightEnemyMatcher = EnemyWithId (asId enemy)}
