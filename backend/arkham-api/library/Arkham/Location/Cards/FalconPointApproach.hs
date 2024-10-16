module Arkham.Location.Cards.FalconPointApproach (falconPointApproach, FalconPointApproach (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FalconPointApproach = FalconPointApproach LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falconPointApproach :: LocationCard FalconPointApproach
falconPointApproach =
  locationWith FalconPointApproach Cards.falconPointApproach 1 (Static 0)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities FalconPointApproach where
  getAbilities (FalconPointApproach a) =
    extendRevealed
      a
      [restricted a 1 (EachUndefeatedInvestigator $ InvestigatorAt (be a)) $ Objective $ forced AnyWindow]

instance RunMessage FalconPointApproach where
  runMessage msg a@(FalconPointApproach attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      actId <- selectJust AnyAct
      push $ AdvanceAct actId (toSource attrs) #other
      pure a
    _ -> FalconPointApproach <$> liftRunMessage msg attrs
