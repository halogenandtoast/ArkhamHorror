module Arkham.Skill.Cards.OnTheBrink2 (onTheBrink2) where

import Arkham.Card
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Matcher
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted
import Arkham.Skill.Types (Field (SkillOwner))
import Data.Map.Strict qualified as Map

newtype OnTheBrink2 = OnTheBrink2 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheBrink2 :: SkillCard OnTheBrink2
onTheBrink2 = skill OnTheBrink2 Cards.onTheBrink2

instance RunMessage OnTheBrink2 where
  runMessage msg s@(OnTheBrink2 attrs) = runQueueT $ case msg of
    FailedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      getSkillTest >>= traverse_ \st -> do
        for_ (Map.assocs st.committedCards) \(iid, cards) -> do
          addToHand iid $ filter ((/= toCardId attrs) . toCardId) cards
        selectEach (NotSkill $ SkillWithId attrs.id) \x -> do
          owner <- field SkillOwner x
          returnToHand owner x

        drawCards attrs.owner attrs 1
      pure s
    _ -> OnTheBrink2 <$> liftRunMessage msg attrs
