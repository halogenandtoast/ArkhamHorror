module Arkham.Treachery.Cards.ChildrenOfBlood.PreyedUpon.FeedingGrounds (feedingGrounds) where

import Arkham.ChaosToken
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestSource)
import Arkham.Modifier
import Arkham.Treachery.CardDefs.ChildrenOfBlood.PreyedUpon qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FeedingGrounds = FeedingGrounds TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedingGrounds :: TreacheryCard FeedingGrounds
feedingGrounds = treachery FeedingGrounds Cards.feedingGrounds

instance RunMessage FeedingGrounds where
  runMessage msg t@(FeedingGrounds attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    RevealChaosToken _ _ token | token.face == BloodToken -> do
      withSkillTestSource \source -> when (isSource attrs source) do
        withSkillTest \sid ->
          skillTestModifier sid attrs (ChaosTokenTarget token) DoNotRevealAnotherChaosToken
        failSkillTest
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> FeedingGrounds <$> liftRunMessage msg attrs
