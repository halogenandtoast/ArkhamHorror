module Arkham.Investigate where

import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Investigate.Types
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source

mkInvestigate
  :: (Sourceable source, HasGame m) => SkillTestId -> InvestigatorId -> source -> m Investigate
mkInvestigate sid iid source = mkInvestigateLocation sid iid source =<< fieldJust InvestigatorLocation iid

withSkillType :: SkillType -> Investigate -> Investigate
withSkillType skillType investigate = investigate {investigateSkillType = skillType}

mkInvestigateLocation
  :: (Sourceable source, HasGame m)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> LocationId
  -> m Investigate
mkInvestigateLocation sid iid source lid = do
  skillType <- field LocationInvestigateSkill lid
  pure
    $ MkInvestigate
      { investigateInvestigator = iid
      , investigateLocation = lid
      , investigateSkillType = skillType
      , investigateSource = toSource source
      , investigateTarget = Nothing
      , investigateIsAction = False
      , investigateSkillTest = sid
      }
