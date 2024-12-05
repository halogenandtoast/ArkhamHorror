module Arkham.Location.Cards.TemplesOfTenochtitlan_176 (
  templesOfTenochtitlan_176,
  TemplesOfTenochtitlan_176 (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getIsBeingInvestigated, getSkillTestInvestigator)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype TemplesOfTenochtitlan_176 = TemplesOfTenochtitlan_176 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templesOfTenochtitlan_176 :: LocationCard TemplesOfTenochtitlan_176
templesOfTenochtitlan_176 = symbolLabel $ location TemplesOfTenochtitlan_176 Cards.templesOfTenochtitlan_176 3 (PerPlayer 1)

instance HasModifiersFor TemplesOfTenochtitlan_176 where
  getModifiersFor (TemplesOfTenochtitlan_176 a) = modifySelfMaybe a do
    iid <- MaybeT getSkillTestInvestigator
    liftGuardM $ fieldP InvestigatorRemainingHealth (<= 3) iid
    liftGuardM $ getIsBeingInvestigated (toId a)
    pure [ShroudModifier (-2)]

instance HasAbilities TemplesOfTenochtitlan_176 where
  getAbilities (TemplesOfTenochtitlan_176 a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ investigatorAt a)
      $ forced
      $ PutLocationIntoPlay #after Anyone (be a)

instance RunMessage TemplesOfTenochtitlan_176 where
  runMessage msg l@(TemplesOfTenochtitlan_176 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) \iid -> directDamage iid (attrs.ability 1) 1
      pure l
    _ -> TemplesOfTenochtitlan_176 <$> liftRunMessage msg attrs
