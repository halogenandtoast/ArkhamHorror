module Arkham.Skill.Cards.LastChance
  ( lastChance
  , LastChance(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner
import Arkham.SkillType
import Arkham.Target

newtype LastChance = LastChance SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lastChance :: SkillCard LastChance
lastChance = skill LastChance Cards.lastChance

instance HasModifiersFor LastChance where
  getModifiersFor (CardIdTarget cid) (LastChance a) | toCardId a == cid = do
    n <- fieldMap InvestigatorHand length (skillOwner a)
    pure $ toModifiers a [RemoveSkillIcons $ replicate n WildIcon]
  getModifiersFor _ _ = pure []

instance RunMessage LastChance where
  runMessage msg (LastChance attrs) = LastChance <$> runMessage msg attrs
