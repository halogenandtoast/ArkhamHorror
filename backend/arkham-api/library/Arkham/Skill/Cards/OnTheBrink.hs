module Arkham.Skill.Cards.OnTheBrink (onTheBrink) where

import Arkham.Card
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Matcher
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Skill.Types (Field (SkillOwner))
import Data.Map.Strict qualified as Map

newtype OnTheBrink = OnTheBrink SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheBrink :: SkillCard OnTheBrink
onTheBrink = skill OnTheBrink Cards.onTheBrink

instance RunMessage OnTheBrink where
  runMessage msg s@(OnTheBrink attrs) = runQueueT $ case msg of
    FailedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      getSkillTest >>= traverse_ \st -> do
        for_ (Map.assocs st.committedCards) \(iid, cards) -> do
          addToHand iid $ filter ((/= toCardId attrs) . toCardId) cards
        selectEach (NotSkill $ SkillWithId attrs.id) \x -> do
          owner <- field SkillOwner x
          returnToHand owner x
      pure s
    _ -> OnTheBrink <$> liftRunMessage msg attrs
