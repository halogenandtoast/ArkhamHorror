module Arkham.Homebrew.DarkMatter.Locations.ShipMainframe (shipMainframe) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (drawEvidence)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ShipMainframe = ShipMainframe LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shipMainframe :: LocationCard ShipMainframe
shipMainframe = location ShipMainframe Cards.shipMainframe 4 (PerPlayer 2)

instance HasModifiersFor ShipMainframe where
  getModifiersFor (ShipMainframe a) = whenUnrevealed a $ modifySelf a [Blocked]

{- | "[action] Parley. Investigators at this location spend 1[per_investigator]
clues, as a group: Draw the top card of the 'Evidence' deck and read it."
-}
instance HasAbilities ShipMainframe where
  getAbilities (ShipMainframe a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ parleyAction
      $ GroupClueCost (PerPlayer 1) (LocationWithId a.id)

instance RunMessage ShipMainframe where
  runMessage msg l@(ShipMainframe attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEvidence iid
      pure l
    _ -> ShipMainframe <$> liftRunMessage msg attrs
