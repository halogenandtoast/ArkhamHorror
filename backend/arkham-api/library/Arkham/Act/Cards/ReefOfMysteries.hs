module Arkham.Act.Cards.ReefOfMysteries (ReefOfMysteries (..), reefOfMysteries) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Key
import Arkham.Matcher

newtype ReefOfMysteries = ReefOfMysteries ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reefOfMysteries :: ActCard ReefOfMysteries
reefOfMysteries = act (1, A) ReefOfMysteries Cards.reefOfMysteries Nothing

instance HasAbilities ReefOfMysteries where
  getAbilities (ReefOfMysteries x) =
    extend1 x
      $ restricted x 1 (foldMap (exists . InvestigatorWithKey) [PurpleKey, WhiteKey, BlackKey])
      $ Objective
      $ forced AnyWindow

instance RunMessage ReefOfMysteries where
  runMessage msg a@(ReefOfMysteries attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> ReefOfMysteries <$> liftRunMessage msg attrs
