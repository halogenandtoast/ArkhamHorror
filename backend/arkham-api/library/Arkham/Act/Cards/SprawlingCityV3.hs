module Arkham.Act.Cards.SprawlingCityV3 (sprawlingCityV3) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (terror)
import Arkham.EncounterSet
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype SprawlingCityV3 = SprawlingCityV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sprawlingCityV3 :: ActCard SprawlingCityV3
sprawlingCityV3 = act (1, A) SprawlingCityV3 Cards.sprawlingCityV3 Nothing

instance HasAbilities SprawlingCityV3 where
  getAbilities (SprawlingCityV3 a) =
    [ restricted a 1 (not_ $ Remembered TheTeamDiscernedTheOriginOfTheShoggoths)
        $ actionAbilityWithCost
        $ SpendTokenKeyCost 2 #tablet
    , restricted a 2 (not_ $ Remembered TheTeamDiscoveredAHiddenPower)
        $ actionAbilityWithCost
        $ SpendTokenKeyCost 2 #cultist
    , restricted
        a
        3
        ( Remembered TheTeamDiscernedTheOriginOfTheShoggoths
            <> Remembered TheTeamDiscoveredAHiddenPower
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SprawlingCityV3 where
  runMessage msg a@(SprawlingCityV3 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      shuffleSetAsideEncounterSet Shoggoths
      shuffleEncounterDiscardBackIn
      hiddenTunnel <- getJustLocationByName "Hidden Tunnel"
      reveal hiddenTunnel
      eachInvestigator (`drawEncounterCard` attrs)
      advanceActDeck attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember TheTeamDiscernedTheOriginOfTheShoggoths
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      remember TheTeamDiscoveredAHiddenPower
      pure a
    _ -> SprawlingCityV3 <$> liftRunMessage msg attrs
