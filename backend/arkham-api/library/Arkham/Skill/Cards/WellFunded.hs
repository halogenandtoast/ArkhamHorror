module Arkham.Skill.Cards.WellFunded (wellFunded, WellFunded (..)) where

import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Matcher
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype WellFunded = WellFunded SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellFunded :: SkillCard WellFunded
wellFunded = skill WellFunded Cards.wellFunded

instance HasModifiersFor WellFunded where
  getModifiersFor (CardIdTarget cid) (WellFunded attrs) | toCardId attrs == cid = do
    assets <- selectCount $ assetControlledBy attrs.owner <> oneOf [#science, #tool]
    if assets > 0
      then modified attrs [AddSkillIcons $ #wild : [#wild | assets >= 3]]
      else pure []
  getModifiersFor _ _ = pure []

instance RunMessage WellFunded where
  runMessage msg (WellFunded attrs) = runQueueT $ case msg of
    _ -> WellFunded <$> liftRunMessage msg attrs
