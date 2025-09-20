module Arkham.Location.Cards.CrumblingPrecipice (crumblingPrecipice) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CrumblingPrecipice = CrumblingPrecipice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crumblingPrecipice :: LocationCard CrumblingPrecipice
crumblingPrecipice = symbolLabel $ location CrumblingPrecipice Cards.crumblingPrecipice 4 (Static 0)

instance HasAbilities CrumblingPrecipice where
  getAbilities (CrumblingPrecipice a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here $ forced $ AttemptExplore #when You

instance RunMessage CrumblingPrecipice where
  runMessage msg l@(CrumblingPrecipice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (IndexedSource 1 $ attrs.ability 1) iid #willpower (Fixed 4)
      pure l
    FailedThisSkillTest iid (IndexedSource n (isAbilitySource attrs 1 -> True)) -> do
      sid <- getRandom
      case n of
        1 -> beginSkillTest sid iid (IndexedSource 2 $ attrs.ability 1) iid #agility (Fixed 3)
        2 -> beginSkillTest sid iid (IndexedSource 3 $ attrs.ability 1) iid #combat (Fixed 2)
        3 -> kill (attrs.ability 1) iid
        _ -> error "Invalid skill type"
      pure l
    _ -> CrumblingPrecipice <$> liftRunMessage msg attrs
