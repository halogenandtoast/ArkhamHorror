module Arkham.Treachery.Cards.Mutated1 (mutated1) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, maybeModifyThisSkillTest)
import Arkham.Investigator.Projection ()
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Message.Lifted.Choose
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Mutated1 = Mutated1 TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mutated1 :: TreacheryCard Mutated1
mutated1 = treachery Mutated1 Cards.mutated1

instance HasModifiersFor Mutated1 where
  getModifiersFor (Mutated1 a) = maybeModifyThisSkillTest a do
    iid <- MaybeT getSkillTestInvestigator
    hasEnemy <- lift $ selectAny $ enemyAtLocationWith iid
    guard hasEnemy
    pure [Difficulty 2]

instance RunMessage Mutated1 where
  runMessage msg t@(Mutated1 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 2)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      mLoc <- field InvestigatorLocation iid
      case mLoc of
        Nothing -> assignDamage iid attrs 2
        Just loc -> do
          investigatorsHere <- select $ InvestigatorAt (LocationWithId loc)
          chooseOneM iid do
            labeled "Take 2 damage" $ assignDamage iid attrs 2
            labeled "Each investigator at your location takes 1 horror" do
              for_ investigatorsHere $ \i -> assignHorror i (toSource attrs) 1
      pure t
    _ -> Mutated1 <$> liftRunMessage msg attrs
