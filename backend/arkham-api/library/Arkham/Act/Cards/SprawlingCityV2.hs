module Arkham.Act.Cards.SprawlingCityV2 (sprawlingCityV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (terror)
import Arkham.EncounterSet
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype SprawlingCityV2 = SprawlingCityV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sprawlingCityV2 :: ActCard SprawlingCityV2
sprawlingCityV2 = act (1, A) SprawlingCityV2 Cards.sprawlingCityV2 Nothing

instance HasAbilities SprawlingCityV2 where
  getAbilities (SprawlingCityV2 a) =
    [ restricted a 1 (not_ $ Remembered TheTeamStudiedTheHistoryOfTheElderThings)
        $ actionAbilityWithCost
        $ SpendTokenKeyCost 2 #elderthing
    , restricted a 2 (not_ $ Remembered TheTeamDiscoveredAHiddenPower)
        $ actionAbilityWithCost
        $ SpendTokenKeyCost 2 #cultist
    , restricted
        a
        3
        ( Remembered TheTeamStudiedTheHistoryOfTheElderThings
            <> Remembered TheTeamDiscoveredAHiddenPower
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SprawlingCityV2 where
  runMessage msg a@(SprawlingCityV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      shuffleSetAsideEncounterSet CreaturesInTheIce
      shuffleEncounterDiscardBackIn
      hiddenTunnel <- getJustLocationByName "Hidden Tunnel"
      reveal hiddenTunnel
      whenM hasRemainingFrostTokens $ addChaosToken #frost
      advanceActDeck attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember TheTeamStudiedTheHistoryOfTheElderThings
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      remember TheTeamDiscoveredAHiddenPower
      pure a
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    _ -> SprawlingCityV2 <$> liftRunMessage msg attrs
