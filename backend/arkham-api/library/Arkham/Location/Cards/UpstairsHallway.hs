module Arkham.Location.Cards.UpstairsHallway (upstairsHallway, UpstairsHallway (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype UpstairsHallway = UpstairsHallway LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

upstairsHallway :: LocationCard UpstairsHallway
upstairsHallway = location UpstairsHallway Cards.upstairsHallway 2 (Static 0)

instance HasModifiersFor UpstairsHallway where
  getModifiersFor (UpstairsHallway a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities UpstairsHallway where
  getAbilities (UpstairsHallway attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 (Here <> notExists (LocationWithTitle "Attic"))
      $ FastAbility
      $ GroupClueCost (PerPlayer 1) (LocationWithId $ toId attrs)

instance RunMessage UpstairsHallway where
  runMessage msg l@(UpstairsHallway attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ PlaceLocationMatching "Attic"
      pure l
    _ -> UpstairsHallway <$> liftRunMessage msg attrs
