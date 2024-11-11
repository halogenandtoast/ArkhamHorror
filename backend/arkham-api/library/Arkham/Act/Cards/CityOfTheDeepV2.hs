module Arkham.Act.Cards.CityOfTheDeepV2 (CityOfTheDeepV2 (..), cityOfTheDeepV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CityOfTheDeepV2 = CityOfTheDeepV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheDeepV2 :: ActCard CityOfTheDeepV2
cityOfTheDeepV2 = act (1, A) CityOfTheDeepV2 Cards.cityOfTheDeepV2 Nothing

instance HasAbilities CityOfTheDeepV2 where
  getAbilities (CityOfTheDeepV2 a) =
    extend
      a
      [ restricted
          a
          1
          ( exists (LocationWithoutClues <> locationIs Locations.lairOfDagonIntoTheMaelstrom)
              <> exists (LocationWithoutClues <> locationIs Locations.lairOfHydra)
              <> exists (LocationWithoutClues <> locationIs Locations.vaultOfRiches <> LocationWithKey PurpleKey)
          )
          $ Objective
          $ forced AnyWindow
      ]

instance RunMessage CityOfTheDeepV2 where
  runMessage msg a@(CityOfTheDeepV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      addToVictory attrs
      otherActs <- selectAny $ NotAct $ ActWithId attrs.id
      if otherActs
        then do
          lead <- getLead
          chooseOneM lead do
            labeled "Continue playing" nothing
            labeled "Proceed immediately to (→R1)" $ push R1
        else push R1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    _ -> CityOfTheDeepV2 <$> liftRunMessage msg attrs
