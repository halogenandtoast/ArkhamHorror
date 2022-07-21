module Arkham.Skill.Cards.LastChance
  ( lastChance
  , LastChance(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Attrs (Field(..))
import Arkham.Projection
import Arkham.SkillType
import Arkham.Skill.Runner

newtype LastChance = LastChance SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lastChance :: SkillCard LastChance
lastChance =
  skill LastChance Cards.lastChance

instance HasModifiersFor LastChance where
  getModifiersFor _ target (LastChance a) | isTarget a target = do
    n <- fieldMap InvestigatorHand length (skillOwner a)
    pure $ toModifiers a [RemoveSkillIcons $ replicate n SkillWild]
  getModifiersFor _ _ _ = pure []

instance RunMessage LastChance where
  runMessage msg (LastChance attrs) = LastChance <$> runMessage msg attrs
