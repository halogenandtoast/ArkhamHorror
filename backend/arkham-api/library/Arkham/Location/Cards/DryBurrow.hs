module Arkham.Location.Cards.DryBurrow (dryBurrow) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.ForMovement
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype DryBurrow = DryBurrow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dryBurrow :: LocationCard DryBurrow
dryBurrow = locationWith DryBurrow Cards.dryBurrow 2 (PerPlayer 1) connectsToAdjacent

instance HasAbilities DryBurrow where
  getAbilities (DryBurrow a) =
    extendRevealed
      a
      [ groupLimit PerRound $ restricted a 1 (Here <> IsDay) $ FastAbility' Free [#move]
      , restricted a 2 (Here <> IsNight) $ forced $ TurnEnds #after You
      ]

instance RunMessage DryBurrow where
  runMessage msg l@(DryBurrow attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <- select $ ConnectedTo ForMovement (be attrs)
      chooseTargetM iid connected $ moveTo (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid (attrs.ability 2) 1
      pure l
    _ -> DryBurrow <$> liftRunMessage msg attrs
