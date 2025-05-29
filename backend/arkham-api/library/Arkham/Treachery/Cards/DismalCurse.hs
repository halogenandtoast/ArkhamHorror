module Arkham.Treachery.Cards.DismalCurse (dismalCurse) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, maybeModifyThisSkillTest)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DismalCurse = DismalCurse TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dismalCurse :: TreacheryCard DismalCurse
dismalCurse = treachery DismalCurse Cards.dismalCurse

instance HasModifiersFor DismalCurse where
  getModifiersFor (DismalCurse a) = maybeModifyThisSkillTest a do
    iid <- MaybeT getSkillTestInvestigator
    guardMatches iid $ InvestigatorWithRemainingSanity (atMost 0)
    pure [Difficulty 2]

instance RunMessage DismalCurse where
  runMessage msg t@(DismalCurse attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      horror <- iid.horror
      sanity <- iid.sanity
      assignDamage iid attrs $ if horror > sanity * 2 then 4 else 2
      pure t
    _ -> DismalCurse <$> liftRunMessage msg attrs
