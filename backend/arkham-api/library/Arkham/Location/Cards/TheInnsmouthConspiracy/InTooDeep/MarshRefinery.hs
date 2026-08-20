module Arkham.Location.Cards.TheInnsmouthConspiracy.InTooDeep.MarshRefinery (marshRefinery) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.InTooDeep qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.TheInnsmouthConspiracy.InTooDeep.Helpers

newtype MarshRefinery = MarshRefinery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marshRefinery :: LocationCard MarshRefinery
marshRefinery = locationWith MarshRefinery Cards.marshRefinery 3 (Static 1) connectsToAdjacent

instance HasAbilities MarshRefinery where
  getAbilities (MarshRefinery a) =
    extendRevealed
      a
      [ restricted
          a
          1
          (Here <> youExist InvestigatorWithAnyKey <> CanMoveTo (ConnectedFrom ForMovement (be a)))
          $ FastAbility Free
      , restricted a 2 Here $ actionAbilityWithCost $ DiscardAssetCost (AssetControlledBy You)
      ]

instance RunMessage MarshRefinery where
  runMessage msg l@(MarshRefinery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      choices <- getConnectedMoveLocations iid (attrs.ability 1)
      chooseTargetM iid choices $ moveTo (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      removeBarrierBetweenConnected iid attrs.id
      pure l
    _ -> MarshRefinery <$> liftRunMessage msg attrs
