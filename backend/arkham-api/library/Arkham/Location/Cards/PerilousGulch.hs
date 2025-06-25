module Arkham.Location.Cards.PerilousGulch (perilousGulch) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype PerilousGulch = PerilousGulch LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perilousGulch :: LocationCard PerilousGulch
perilousGulch = symbolLabel $ location PerilousGulch Cards.perilousGulch 4 (PerPlayer 1)

instance HasAbilities PerilousGulch where
  getAbilities (PerilousGulch a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be a) #failure

instance RunMessage PerilousGulch where
  runMessage msg l@(PerilousGulch attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      hasRope <- selectAny $ investigatorAt (toId attrs) <> InvestigatorWithSupply Rope
      unless hasRope $ placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> PerilousGulch <$> liftRunMessage msg attrs
