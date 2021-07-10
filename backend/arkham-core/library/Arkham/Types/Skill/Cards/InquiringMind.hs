module Arkham.Types.Skill.Cards.InquiringMind
  ( inquiringMind
  , InquiringMind(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype InquiringMind = InquiringMind SkillAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inquiringMind :: SkillCard InquiringMind
inquiringMind = skill InquiringMind Cards.inquiringMind

instance HasModifiersFor env InquiringMind

instance HasActions env InquiringMind where
  getActions iid window (InquiringMind attrs) = getActions iid window attrs

instance SkillRunner env => RunMessage env InquiringMind where
  runMessage msg (InquiringMind attrs) = InquiringMind <$> runMessage msg attrs
