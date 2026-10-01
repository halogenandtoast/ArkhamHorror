module Arkham.Homebrew.CircusExMortis.Locations.PrimalForest (primalForest) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (scatterTowardSilentClearing, shubNiggurathLeaves)
import Arkham.Location.Import.Lifted

newtype PrimalForest = PrimalForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primalForest :: LocationCard PrimalForest
primalForest = location PrimalForest Cards.primalForest 10 (Static 0)

{- | One Forced per face, because the two faces dispose of the location differently:
the revealed face "moves this location to the victory display", the unrevealed one
"removes this location from the game". Keeping them as separate abilities means each
face offers only its own, rather than one ability branching on 'revealed' at resolution.
-}
instance HasAbilities PrimalForest where
  getAbilities (PrimalForest a) =
    extendRevealed1 a (shubNiggurathLeaves 1 a) <> extendUnrevealed1 a (shubNiggurathLeaves 2 a)

instance RunMessage PrimalForest where
  runMessage msg l@(PrimalForest attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      scatterTowardSilentClearing attrs
      addToVictory_ attrs
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      scatterTowardSilentClearing attrs
      -- "Remove this location from the game" -- which must skip 'removeLocation''s
      -- victory diversion, or an unrevealed Primal Forest would score its Victory 1.
      removeLocationWithoutVictory attrs
      pure l
    _ -> PrimalForest <$> liftRunMessage msg attrs
