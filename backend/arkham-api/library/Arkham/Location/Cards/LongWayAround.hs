module Arkham.Location.Cards.LongWayAround (longWayAround, longWayAroundEffect, LongWayAround (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers

newtype LongWayAround = LongWayAround LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

longWayAround :: LocationCard LongWayAround
longWayAround =
  locationWith LongWayAround Cards.longWayAround 6 (Static 0)
    $ connectsToL
    .~ setFromList [LeftOf, RightOf]

instance HasAbilities LongWayAround where
  getAbilities (LongWayAround a) =
    extendRevealed
      a
      [ mkAbility a 1 $ SilentForcedAbility $ RevealLocation #after Anyone (be a)
      , mkAbility a 2 $ forced $ VehicleEnters #after AnyAsset (be a)
      ]

instance RunMessage LongWayAround where
  runMessage msg l@(LongWayAround attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      road 1 attrs
      pure l
    UseCardAbility _iid (isSource attrs -> True) 2 (getEnteringVehicle -> vehicle) _ -> do
      placeDoomOnAgendaAndCheckAdvance 1
      createCardEffect Cards.longWayAround Nothing (attrs.ability 2) vehicle
      pure l
    _ -> LongWayAround <$> liftRunMessage msg attrs

newtype LongWayAroundEffect = LongWayAroundEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LongWayAroundEffect where
  getModifiersFor (LongWayAroundEffect a) = modified_ a a.target [VehicleCannotMove]

longWayAroundEffect :: EffectArgs -> LongWayAroundEffect
longWayAroundEffect = cardEffect LongWayAroundEffect Cards.longWayAround

instance RunMessage LongWayAroundEffect where
  runMessage msg e@(LongWayAroundEffect attrs) = runQueueT $ case msg of
    EndRound ->
      if toResultDefault False attrs.extra
        then disableReturn e
        else pure . LongWayAroundEffect $ setEffectMeta True attrs
    _ -> LongWayAroundEffect <$> liftRunMessage msg attrs
