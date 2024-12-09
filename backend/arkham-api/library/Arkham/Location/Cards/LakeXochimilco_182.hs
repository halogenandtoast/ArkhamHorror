module Arkham.Location.Cards.LakeXochimilco_182 (lakeXochimilco_182, LakeXochimilco_182 (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getIsBeingInvestigated, getSkillTestInvestigator)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype LakeXochimilco_182 = LakeXochimilco_182 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lakeXochimilco_182 :: LocationCard LakeXochimilco_182
lakeXochimilco_182 = symbolLabel $ location LakeXochimilco_182 Cards.lakeXochimilco_182 2 (PerPlayer 1)

instance HasModifiersFor LakeXochimilco_182 where
  getModifiersFor (LakeXochimilco_182 a) = modifySelfMaybe a do
    iid <- MaybeT getSkillTestInvestigator
    actionsRemaining <- lift $ field InvestigatorRemainingActions iid
    guard $ actionsRemaining > 0
    liftGuardM $ getIsBeingInvestigated (toId a)
    pure [ShroudModifier (2 * actionsRemaining)]

instance HasAbilities LakeXochimilco_182 where
  getAbilities (LakeXochimilco_182 attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 (exists $ investigatorAt attrs)
      $ forced
      $ PutLocationIntoPlay #after Anyone (be attrs)

instance RunMessage LakeXochimilco_182 where
  runMessage msg l@(LakeXochimilco_182 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- select $ investigatorAt (toId attrs)
      pushAll [SetActions iid (attrs.ability 1) 0 | iid <- iids]
      pure l
    _ -> LakeXochimilco_182 <$> liftRunMessage msg attrs
