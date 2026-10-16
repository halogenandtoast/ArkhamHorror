{- | "Revelation - Put Supply and Demand into play next to the agenda deck."

The __Forced__ trigger is one window with two arms: an investigation, or a test
from an {action} ability on a scenario card. The second arm is the matcher twin
of 'testIsFromScenarioActionAbility' -- an ability source whose own source is an
encounter card, with an {action} ability type -- which is what a window matcher
can say in pure 'getAbilities'.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Treacheries.SupplyAndDemand (
  supplyAndDemand,
) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype SupplyAndDemand = SupplyAndDemand TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

supplyAndDemand :: TreacheryCard SupplyAndDemand
supplyAndDemand = treachery SupplyAndDemand Cards.supplyAndDemand

tolledTests :: SkillTestMatcher
tolledTests =
  SkillTestOneOf
    [ #investigation
    , SkillTestSourceMatches
        $ SourceIsAbility (AbilityIsActionAbility <> AbilityOnEncounterCard)
    ]

instance HasAbilities SupplyAndDemand where
  getAbilities (SupplyAndDemand a) =
    [ limited (MaxPer Cards.supplyAndDemand PerTest 1)
        $ scenarioI18n
        $ withI18nTooltip "supplyAndDemand.toll"
        $ mkAbility a 1
        $ forced
        $ InitiatedSkillTest #when You #any #any tolledTests
    ]

instance RunMessage SupplyAndDemand where
  runMessage msg t@(SupplyAndDemand attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) sid (Difficulty 2)
        onFailedByEffect sid (atLeast 0) (attrs.ability 1) iid
          $ loseResources iid (attrs.ability 1) 1
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> SupplyAndDemand <$> liftRunMessage msg attrs
