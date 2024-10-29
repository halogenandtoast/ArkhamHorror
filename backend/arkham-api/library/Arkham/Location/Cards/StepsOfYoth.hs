module Arkham.Location.Cards.StepsOfYoth (stepsOfYoth, StepsOfYoth (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype StepsOfYoth = StepsOfYoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfYoth :: LocationCard StepsOfYoth
stepsOfYoth = symbolLabel $ location StepsOfYoth Cards.stepsOfYoth 3 (Static 0)

instance HasAbilities StepsOfYoth where
  getAbilities (StepsOfYoth attrs) =
    extendRevealed1 attrs
      $ groupLimit PerGame
      $ restrictedAbility
        attrs
        1
        ( Here
            <> HasCalculation
              (InvestigatorsFieldCalculation (investigatorAt attrs) InvestigatorClues)
              (AtLeast $ PerPlayer 5)
        )
      $ ReactionAbility AddingToCurrentDepth
      $ SupplyCost (be attrs) Rope

instance RunMessage StepsOfYoth where
  runMessage msg l@(StepsOfYoth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pushAll =<< incrementDepth
      pure l
    _ -> StepsOfYoth <$> liftRunMessage msg attrs
