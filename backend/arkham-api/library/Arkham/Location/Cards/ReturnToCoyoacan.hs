module Arkham.Location.Cards.ReturnToCoyoacan (returnToCoyoacan) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (InvestigatorDamage)
import Arkham.Matcher

newtype ReturnToCoyoacan = ReturnToCoyoacan LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToCoyoacan :: LocationCard ReturnToCoyoacan
returnToCoyoacan = symbolLabel $ location ReturnToCoyoacan Cards.returnToCoyoacan 1 (Static 0)

instance HasModifiersFor ReturnToCoyoacan where
  getModifiersFor (ReturnToCoyoacan a) = do
    modifySelect a (investigator_ $ at_ (be a)) [CannotHaveHorrorHealed, CannotHaveDamageHealed]
    modifySelect a (TargetControlledBy $ at_ (be a)) [CannotHaveHorrorHealed, CannotHaveDamageHealed]

instance HasAbilities ReturnToCoyoacan where
  getAbilities (ReturnToCoyoacan a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( Here
            <> HasCalculation
              (SumCalculation $ map (InvestigatorsFieldCalculation You) [InvestigatorDamage, InvestigatorHorror])
              (atLeast 5)
        )
        exploreAction_

instance RunMessage ReturnToCoyoacan where
  runMessage msg l@(ReturnToCoyoacan attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Explore iid (attrs.ability 1) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> ReturnToCoyoacan <$> liftRunMessage msg attrs
