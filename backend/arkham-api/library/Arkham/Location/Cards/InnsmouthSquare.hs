module Arkham.Location.Cards.InnsmouthSquare (innsmouthSquare, InnsmouthSquare (..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (resignAction)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers
import Arkham.Trait (Trait (Innsmouth))

newtype InnsmouthSquare = InnsmouthSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthSquare :: LocationCard InnsmouthSquare
innsmouthSquare = location InnsmouthSquare Cards.innsmouthSquare 4 (PerPlayer 1)

instance HasAbilities InnsmouthSquare where
  getAbilities (InnsmouthSquare a) =
    extendRevealed
      a
      [ scenarioTooltip "innsmouthSquare.resign" $ resignAction a
      , scenarioTooltip "innsmouthSquare.ability2" $ restricted a 2 Here (FastAbility $ ResourceCost 2)
      ]

instance RunMessage InnsmouthSquare where
  runMessage msg l@(InnsmouthSquare attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      connected <- select $ ConnectedTo (be attrs) <> withTrait Innsmouth
      chooseTargetM iid connected $ moveTo (attrs.ability 2) iid
      pure l
    _ -> InnsmouthSquare <$> liftRunMessage msg attrs
