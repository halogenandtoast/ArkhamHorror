module Arkham.Act.Cards.CityOfTheDeepV1 (CityOfTheDeepV1 (..), cityOfTheDeepV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (AncientOne, Lair, Sanctum))

newtype CityOfTheDeepV1 = CityOfTheDeepV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheDeepV1 :: ActCard CityOfTheDeepV1
cityOfTheDeepV1 = act (1, A) CityOfTheDeepV1 Cards.cityOfTheDeepV1 Nothing

instance HasAbilities CityOfTheDeepV1 where
  getAbilities (CityOfTheDeepV1 a) =
    extend
      a
      [ restricted a 1 (exists $ not_ (withTrait Sanctum) <> FloodedLocation)
          $ FastAbility
          $ OrCost
            [ RemoveEnemyDamageCost (PerPlayer 1) (EnemyAt YourLocation <> withTrait AncientOne)
            , GroupClueCost (PerPlayer 1) (withTrait Lair)
            ]
      , restricted a 2 (Negate $ exists FloodedLocation) $ Objective $ forced AnyWindow
      ]

instance RunMessage CityOfTheDeepV1 where
  runMessage msg a@(CityOfTheDeepV1 attrs) = runQueueT $ case msg of
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
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <- select $ FloodedLocation <> not_ (withTrait Sanctum)
      chooseTargetM iid ls decreaseThisFloodLevel
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> CityOfTheDeepV1 <$> liftRunMessage msg attrs
