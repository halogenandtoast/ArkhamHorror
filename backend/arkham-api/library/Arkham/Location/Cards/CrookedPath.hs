module Arkham.Location.Cards.CrookedPath (crookedPath) where

import Arkham.Ability
import Arkham.Helpers.Location (getCanMoveTo)
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.TheTwistedHollow.Helpers

newtype CrookedPath = CrookedPath LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crookedPath :: LocationCard CrookedPath
crookedPath = locationWith CrookedPath Cards.crookedPath 0 (Static 1) connectsToAdjacent

instance HasModifiersFor CrookedPath where
  getModifiersFor (CrookedPath a) = do
    x <- getDarknessLevel
    modifySelf a [ShroudModifier x]

instance HasAbilities CrookedPath where
  getAbilities (CrookedPath a) =
    extendRevealed a [mkAbility a 1 $ freeReaction (DiscoverClues #after You (be a) (atLeast 1))]

instance RunMessage CrookedPath where
  runMessage msg l@(CrookedPath attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ connectedFrom (locationWithInvestigator iid) <> UnrevealedLocation
      chooseTargetM iid locations \loc -> do
        lookAtRevealed iid (attrs.ability 1) loc
        whenM (getCanMoveTo iid (attrs.ability 1) loc) do
          chooseOneM iid $ withI18n do
            labeled' "move" $ moveTo (attrs.ability 1) iid loc
            skip_
      pure l
    _ -> CrookedPath <$> liftRunMessage msg attrs
