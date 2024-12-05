module Arkham.Location.Cards.FalconPointGatehouse (falconPointGatehouse, FalconPointGatehouse (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Projection ()
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype FalconPointGatehouse = FalconPointGatehouse LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falconPointGatehouse :: LocationCard FalconPointGatehouse
falconPointGatehouse = location FalconPointGatehouse Cards.falconPointGatehouse 1 (Static 0)

instance HasModifiersFor FalconPointGatehouse where
  getModifiersFor (FalconPointGatehouse attrs) = modifySelf attrs [CannotBeFullyFlooded]

instance HasAbilities FalconPointGatehouse where
  getAbilities (FalconPointGatehouse a) =
    extendRevealed
      a
      [ withTooltip "You head back into the woods, leaving the lighthouse and its mysteries behind."
          $ restricted a 1 Here
          $ ActionAbility [#resign] (ActionCost 1)
      ]

instance RunMessage FalconPointGatehouse where
  runMessage msg l@(FalconPointGatehouse attrs) = runQueueT $ case msg of
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      act <- selectJust AnyAct
      resign iid
      ks <- iid.keys
      for_ ks \k -> when (k `elem` [RedKey, BlackKey]) $ placeKey act k
      pure l
    _ -> FalconPointGatehouse <$> liftRunMessage msg attrs
