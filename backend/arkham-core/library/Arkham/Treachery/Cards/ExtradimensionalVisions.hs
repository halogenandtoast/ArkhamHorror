module Arkham.Treachery.Cards.ExtradimensionalVisions (
  extradimensionalVisions,
  ExtradimensionalVisions (..),
) where

import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ExtradimensionalVisions = ExtradimensionalVisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

extradimensionalVisions :: TreacheryCard ExtradimensionalVisions
extradimensionalVisions =
  treachery ExtradimensionalVisions Cards.extradimensionalVisions

instance RunMessage ExtradimensionalVisions where
  runMessage msg t@(ExtradimensionalVisions attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push
        $ revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 2, DividedByCalculation (ScenarioInDiscardCountCalculation AnyCard) 10]
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ ChooseAndDiscardAsset iid (toSource attrs) AnyAsset
      pure t
    _ -> ExtradimensionalVisions <$> runMessage msg attrs
