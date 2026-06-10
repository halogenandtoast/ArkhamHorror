module Arkham.Location.Cards.OutskirtsOfCairo (outskirtsOfCairo) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype OutskirtsOfCairo = OutskirtsOfCairo LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outskirtsOfCairo :: LocationCard OutskirtsOfCairo
outskirtsOfCairo = location OutskirtsOfCairo Cards.outskirtsOfCairo 4 (PerPlayer 1)

instance HasAbilities OutskirtsOfCairo where
  getAbilities (OutskirtsOfCairo a) =
    extendRevealed
      a
      [ restricted a 1 Here $ actionAbilityWithCost (HandDiscardCost 5 #any)
      , restricted a 2 (Here <> Negate (Remembered SabotagedTheTrain))
          $ ActionAbility #resign Nothing (ActionCost 1)
      ]

instance RunMessage OutskirtsOfCairo where
  runMessage msg l@(OutskirtsOfCairo attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember SabotagedTheTrain
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resign iid
      pure l
    _ -> OutskirtsOfCairo <$> liftRunMessage msg attrs
