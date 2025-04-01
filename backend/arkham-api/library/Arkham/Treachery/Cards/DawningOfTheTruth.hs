module Arkham.Treachery.Cards.DawningOfTheTruth (dawningOfTheTruth) where

import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DawningOfTheTruth = DawningOfTheTruth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dawningOfTheTruth :: TreacheryCard DawningOfTheTruth
dawningOfTheTruth = treachery DawningOfTheTruth Cards.dawningOfTheTruth

instance RunMessage DawningOfTheTruth where
  runMessage msg t@(DawningOfTheTruth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation
          [ Fixed 2
          , InvestigatorKeyCountCalculation (InvestigatorWithId iid)
          , LocationKeyCountCalculation (locationWithInvestigator iid)
          ]
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid attrs $ if n >= 3 then 3 else 2
      pure t
    _ -> DawningOfTheTruth <$> liftRunMessage msg attrs
