module Arkham.Location.Cards.TheFarmhouse (theFarmhouse) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.ForMovement
import Arkham.Helpers.Modifiers (modifySelfWith)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Scenarios.TheLongestNight.Helpers

newtype TheFarmhouse = TheFarmhouse LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFarmhouse :: LocationCard TheFarmhouse
theFarmhouse = symbolLabel $ locationWith TheFarmhouse Cards.theFarmhouse 4 (PerPlayer 2) connectsToAdjacent

instance HasModifiersFor TheFarmhouse where
  getModifiersFor (TheFarmhouse a) =
    modifySelfWith a setActiveDuringSetup [CannotHaveAttachments, CannotHaveTraps, CannotHaveDecoys]

instance HasAbilities TheFarmhouse where
  getAbilities (TheFarmhouse a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> DuringTurn You)
          $ FastAbility' (CalculatedResourceCost $ GameValueCalculation $ PerPlayer 1) #move
      , groupLimit PerRound $ restricted a 2 Here actionAbility
      ]

instance RunMessage TheFarmhouse where
  runMessage msg l@(TheFarmhouse attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <- select $ ConnectedTo ForMovement (be attrs)
      chooseTargetM iid connected $ moveTo (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      gainClues iid (attrs.ability 2) 1
      pure l
    _ -> TheFarmhouse <$> liftRunMessage msg attrs
