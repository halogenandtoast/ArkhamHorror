module Arkham.Act.Cards.TheDrownedCity.ObsidianCanyons.ScouringTheSpires (scouringTheSpires) where

import Arkham.Ability
import Arkham.Act.CardDefs.TheDrownedCity.ObsidianCanyons qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.CardDefs.TheDrownedCity.ObsidianCanyons qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheDrownedCity.ObsidianCanyons.Helpers

newtype ScouringTheSpires = ScouringTheSpires ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scouringTheSpires :: ActCard ScouringTheSpires
scouringTheSpires = act (1, A) ScouringTheSpires Cards.scouringTheSpires Nothing

instance HasAbilities ScouringTheSpires where
  getAbilities (ScouringTheSpires a) =
    extend
      a
      [ restricted a 1 (ScenarioDeckWithCard SummitDeck) $ actionAbilityWithCost ClueCostX
      , onlyOnce
          $ restricted
            a
            2
            ( exists UneliminatedInvestigator
                <> EachUndefeatedInvestigator (at_ $ locationIs Locations.centralSpire <> RevealedLocation)
            )
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage ScouringTheSpires where
  runMessage msg a@(ScouringTheSpires attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalCluePayment -> n) -> do
      searchTheSpires (attrs.ability 1) iid n
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      selectEach (locationIs Locations.rlyehStreets) removeIgnoringTextBox
      centralSpire <- selectJust $ locationIs Locations.centralSpire
      rebuildSkyline centralSpire actTwoLayout
      scenarioSpecific "shuffleScouringAct2Summit" ()
      advanceActDeck attrs
      pure a
    _ -> ScouringTheSpires <$> liftRunMessage msg attrs
