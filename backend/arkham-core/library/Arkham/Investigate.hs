module Arkham.Investigate
where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Investigate.Types
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

mkInvestigate :: (Sourceable source, HasGame m) => InvestigatorId -> source -> m Investigate
mkInvestigate iid source = mkInvestigateLocation iid source =<< fieldJust InvestigatorLocation iid

withSkillType :: SkillType -> Investigate -> Investigate
withSkillType skillType investigate = investigate {investigateSkillType = skillType}

mkInvestigateLocation
  :: (Sourceable source, HasGame m) => InvestigatorId -> source -> LocationId -> m Investigate
mkInvestigateLocation iid source lid = do
  skillType <- field LocationInvestigateSkill lid
  pure
    $ MkInvestigate
      { investigateInvestigator = iid
      , investigateLocation = lid
      , investigateSkillType = skillType
      , investigateSource = toSource source
      , investigateTarget = Nothing
      , investigateIsAction = False
      }
