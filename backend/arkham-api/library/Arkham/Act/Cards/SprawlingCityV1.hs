module Arkham.Act.Cards.SprawlingCityV1 (sprawlingCityV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (terror)
import Arkham.EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype SprawlingCityV1 = SprawlingCityV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sprawlingCityV1 :: ActCard SprawlingCityV1
sprawlingCityV1 = act (1, A) SprawlingCityV1 Cards.sprawlingCityV1 Nothing

instance HasAbilities SprawlingCityV1 where
  getAbilities (SprawlingCityV1 a) =
    [ restricted a 1 (not_ $ Remembered TheTeamStudiedTheHistoryOfTheElderThings)
        $ actionAbilityWithCost
        $ SpendTokenKeyCost 2 #elderthing
    , restricted a 2 (not_ $ Remembered TheTeamDiscernedTheOriginOfTheShoggoths)
        $ actionAbilityWithCost
        $ SpendTokenKeyCost 2 #tablet
    , restricted
        a
        3
        ( Remembered TheTeamStudiedTheHistoryOfTheElderThings
            <> Remembered TheTeamDiscernedTheOriginOfTheShoggoths
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SprawlingCityV1 where
  runMessage msg a@(SprawlingCityV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      shuffleSetAsideEncounterSet Shoggoths
      shuffleEncounterDiscardBackIn
      hiddenTunnel <- getJustLocationByName "Hidden Tunnel"
      reveal hiddenTunnel
      terror <- getSetAsideCard Enemies.terrorOfTheStarsBaneOfTheElderThings
      createEnemyWith_ terror hiddenTunnel createExhausted
      advanceActDeck attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      remember TheTeamStudiedTheHistoryOfTheElderThings
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      remember TheTeamDiscernedTheOriginOfTheShoggoths
      pure a
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    _ -> SprawlingCityV1 <$> liftRunMessage msg attrs
