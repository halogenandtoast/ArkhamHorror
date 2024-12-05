module Arkham.Location.Cards.UpstairsDoorwayLibrary (upstairsDoorwayLibrary, UpstairsDoorwayLibrary (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype UpstairsDoorwayLibrary = UpstairsDoorwayLibrary LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

upstairsDoorwayLibrary :: LocationCard UpstairsDoorwayLibrary
upstairsDoorwayLibrary = location UpstairsDoorwayLibrary Cards.upstairsDoorwayLibrary 1 (PerPlayer 1)

instance HasModifiersFor UpstairsDoorwayLibrary where
  getModifiersFor (UpstairsDoorwayLibrary a) =
    whenRevealed a
      $ modifySelf
        a
        [AdditionalCostToInvestigate (OrCost [ActionCost 1, HorrorCost (toSource a) YouTarget 1])]

instance HasAbilities UpstairsDoorwayLibrary where
  getAbilities (UpstairsDoorwayLibrary a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> notExists (LocationWithTitle "Unmarked Tomb"))
      $ FastAbility (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage UpstairsDoorwayLibrary where
  runMessage msg l@(UpstairsDoorwayLibrary attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ PlaceLocationMatching "Unmarked Tomb"
      pure l
    _ -> UpstairsDoorwayLibrary <$> liftRunMessage msg attrs
