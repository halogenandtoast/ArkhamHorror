module Arkham.Treachery.Cards.FinalMistake (finalMistake) where

import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FinalMistake = FinalMistake TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finalMistake :: TreacheryCard FinalMistake
finalMistake = treachery FinalMistake Cards.finalMistake

instance RunMessage FinalMistake where
  runMessage msg t@(FinalMistake attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      doom <- getSum <$> selectAgg Sum LocationDoom (locationWithInvestigator iid)
      sid <- getRandom
      skillTestModifier sid attrs (SkillTestTarget sid) (Difficulty doom)
      revelationSkillTest sid iid attrs #agility (Fixed 2)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> FinalMistake <$> liftRunMessage msg attrs
