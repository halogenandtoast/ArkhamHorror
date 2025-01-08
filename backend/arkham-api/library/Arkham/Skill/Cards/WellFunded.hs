module Arkham.Skill.Cards.WellFunded (wellFunded, WellFunded (..)) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype WellFunded = WellFunded SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellFunded :: SkillCard WellFunded
wellFunded = skill WellFunded Cards.wellFunded

instance HasModifiersFor WellFunded where
  getModifiersFor (WellFunded attrs) = do
    assets <- selectCount $ assetControlledBy attrs.owner <> oneOf [#science, #tool]
    modifiedWhen_ attrs (assets > 0) attrs.cardId [AddSkillIcons $ #wild : [#wild | assets >= 3]]

instance RunMessage WellFunded where
  runMessage msg (WellFunded attrs) = runQueueT $ case msg of
    _ -> WellFunded <$> liftRunMessage msg attrs
