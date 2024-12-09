module Arkham.Location.Cards.LakeXochimilco_183 (lakeXochimilco_183, LakeXochimilco_183 (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getIsBeingInvestigated, getSkillTestInvestigator)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype LakeXochimilco_183 = LakeXochimilco_183 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lakeXochimilco_183 :: LocationCard LakeXochimilco_183
lakeXochimilco_183 = symbolLabel $ location LakeXochimilco_183 Cards.lakeXochimilco_183 4 (PerPlayer 2)

instance HasModifiersFor LakeXochimilco_183 where
  getModifiersFor (LakeXochimilco_183 a) = modifySelfMaybe a do
    iid <- MaybeT getSkillTestInvestigator
    liftGuardM $ fieldP InvestigatorRemainingSanity (<= 3) iid
    liftGuardM $ getIsBeingInvestigated (toId a)
    pure [ShroudModifier (-2)]

instance HasAbilities LakeXochimilco_183 where
  getAbilities (LakeXochimilco_183 attrs) =
    extendRevealed1 attrs
      $ restrictedAbility attrs 1 (exists $ investigatorAt attrs)
      $ forced
      $ PutLocationIntoPlay #after Anyone (be attrs)

instance RunMessage LakeXochimilco_183 where
  runMessage msg l@(LakeXochimilco_183 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs.id) \iid -> directHorror iid (attrs.ability 1) 1
      pure l
    _ -> LakeXochimilco_183 <$> liftRunMessage msg attrs
