module Arkham.Homebrew.TheLegendOfSleepyHollow.Locations.VanTasselFarmstead (
  vanTasselFarmstead,
) where

import Arkham.Ability
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorResources))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)
import Arkham.Projection

newtype VanTasselFarmstead = VanTasselFarmstead LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vanTasselFarmstead :: LocationCard VanTasselFarmstead
vanTasselFarmstead = location VanTasselFarmstead Cards.vanTasselFarmstead 3 (PerPlayer 1)

instance HasAbilities VanTasselFarmstead where
  getAbilities (VanTasselFarmstead a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "vanTasselFarmstead.hospitality"
          $ playerLimit PerTurn
          $ restricted
            a
            1
            ( Here
                <> youExist (HandWith (LengthIs $ atMost 2) <> InvestigatorWithResources (atMost 2))
            )
            actionAbility
      , scenarioI18n
          $ withI18nTooltip "vanTasselFarmstead.parley"
          $ skillTestAbility
          $ restricted a 2 Here #parley
      ]

instance RunMessage VanTasselFarmstead where
  runMessage msg l@(VanTasselFarmstead attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      gainResources iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resources <- field InvestigatorResources iid
      scenarioI18n
        $ scope "vanTasselFarmstead"
        $ countVar resources
        $ chooseAmount iid "resourcesToSpend" "$resources" 0 resources attrs
      pure l
    -- "reduce this test's difficulty by 1 for every 2 resources spent"
    ResolveAmounts iid (getChoiceAmount "$resources" -> spent) (isTarget attrs -> True) -> do
      spendResources iid spent
      sid <- getRandom
      parley sid iid (attrs.ability 2) attrs #willpower (Fixed $ max 0 (7 - spent `div` 2))
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      remember interviewedVanBruntsStaff
      pure l
    _ -> VanTasselFarmstead <$> liftRunMessage msg attrs
