module Arkham.Homebrew.TheLegendOfSleepyHollow.Locations.DerelictSchoolhouse (
  derelictSchoolhouse,
) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)

newtype DerelictSchoolhouse = DerelictSchoolhouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

derelictSchoolhouse :: LocationCard DerelictSchoolhouse
derelictSchoolhouse = location DerelictSchoolhouse Cards.derelictSchoolhouse 3 (PerPlayer 3)

instance HasAbilities DerelictSchoolhouse where
  getAbilities (DerelictSchoolhouse a) =
    extendRevealed
      a
      [ scenarioI18n $ hauntedI "derelictSchoolhouse.haunted" a 1
      , scenarioI18n
          $ withI18nTooltip "derelictSchoolhouse.history"
          $ restricted a 2 Here
          $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
      ]

instance RunMessage DerelictSchoolhouse where
  runMessage msg l@(DerelictSchoolhouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      remember readTheHollowsHistory
      pure l
    _ -> DerelictSchoolhouse <$> liftRunMessage msg attrs
