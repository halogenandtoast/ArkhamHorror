module Arkham.Location.Cards.SacredWoods_185 (sacredWoods_185) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SacredWoods_185 = SacredWoods_185 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sacredWoods_185 :: LocationCard SacredWoods_185
sacredWoods_185 = symbolLabel $ location SacredWoods_185 Cards.sacredWoods_185 6 (PerPlayer 1)

instance HasModifiersFor SacredWoods_185 where
  getModifiersFor (SacredWoods_185 a) = whenRevealed a $ do
    modifySelect a (investigatorAt a) [IncreaseCostOf (basic AnyCard) 2]
    maybeModifySelf a do
      iid <- MaybeT getSkillTestInvestigator
      liftGuardM $ getIsBeingInvestigated a.id
      n <- lift $ selectCount $ assetControlledBy iid
      pure [ShroudModifier (-n)]

instance RunMessage SacredWoods_185 where
  runMessage msg (SacredWoods_185 attrs) = SacredWoods_185 <$> runMessage msg attrs
