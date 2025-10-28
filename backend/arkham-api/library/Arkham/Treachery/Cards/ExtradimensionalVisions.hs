module Arkham.Treachery.Cards.ExtradimensionalVisions (extradimensionalVisions) where

import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ExtradimensionalVisions = ExtradimensionalVisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extradimensionalVisions :: TreacheryCard ExtradimensionalVisions
extradimensionalVisions = treachery ExtradimensionalVisions Cards.extradimensionalVisions

instance RunMessage ExtradimensionalVisions where
  runMessage msg t@(ExtradimensionalVisions attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 2, DividedByCalculation (ScenarioInDiscardCountCalculation AnyCard) 10]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseAndDiscardAsset iid (toSource attrs)
      pure t
    _ -> ExtradimensionalVisions <$> liftRunMessage msg attrs
