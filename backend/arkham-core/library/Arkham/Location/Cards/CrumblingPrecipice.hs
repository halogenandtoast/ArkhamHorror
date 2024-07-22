module Arkham.Location.Cards.CrumblingPrecipice (crumblingPrecipice, CrumblingPrecipice (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillTest.Type
import Arkham.SkillType

newtype CrumblingPrecipice = CrumblingPrecipice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crumblingPrecipice :: LocationCard CrumblingPrecipice
crumblingPrecipice = symbolLabel $ location CrumblingPrecipice Cards.crumblingPrecipice 4 (Static 0)

instance HasAbilities CrumblingPrecipice where
  getAbilities (CrumblingPrecipice a) =
    withRevealedAbilities a [restrictedAbility a 1 Here $ forced $ AttemptExplore #when You]

instance RunMessage CrumblingPrecipice where
  runMessage msg l@(CrumblingPrecipice attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 4)
      pure l
    FailedSkillTest iid _ (isAbilitySource attrs 1 -> True) Initiator {} (SkillSkillTest sType) _ -> do
      sid <- getRandom
      case sType of
        SkillWillpower -> push $ beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
        SkillAgility -> push $ beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 2)
        SkillCombat -> push $ InvestigatorKilled (toSource attrs) iid
        _ -> error "Invalid skill type"
      pure l
    _ -> CrumblingPrecipice <$> runMessage msg attrs
