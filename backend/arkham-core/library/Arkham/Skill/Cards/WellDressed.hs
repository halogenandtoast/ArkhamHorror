module Arkham.Skill.Cards.WellDressed (wellDressed, WellDressed (..)) where

import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest (getSkillTestAction)
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype WellDressed = WellDressed SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellDressed :: SkillCard WellDressed
wellDressed = skill WellDressed Cards.wellDressed

instance HasModifiersFor WellDressed where
  getModifiersFor (CardIdTarget cid) (WellDressed attrs) | toCardId attrs == cid = do
    maybeModified attrs do
      Action.Parley <- MaybeT getSkillTestAction
      pure [AddSkillIcons [#wild, #wild, #wild]]
  getModifiersFor _ _ = pure []

instance RunMessage WellDressed where
  runMessage msg (WellDressed attrs) = runQueueT $ case msg of
    _ -> WellDressed <$> liftRunMessage msg attrs
