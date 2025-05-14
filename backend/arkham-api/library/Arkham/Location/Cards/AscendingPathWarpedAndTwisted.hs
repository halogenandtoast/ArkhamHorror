module Arkham.Location.Cards.AscendingPathWarpedAndTwisted (ascendingPathWarpedAndTwisted) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AscendingPathWarpedAndTwisted = AscendingPathWarpedAndTwisted LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendingPathWarpedAndTwisted :: LocationCard AscendingPathWarpedAndTwisted
ascendingPathWarpedAndTwisted =
  locationWith AscendingPathWarpedAndTwisted Cards.ascendingPathWarpedAndTwisted 4 (Static 3)
    $ revealedConnectedMatchersL
    <>~ ["Altered Path"]

instance HasModifiersFor AscendingPathWarpedAndTwisted where
  getModifiersFor (AscendingPathWarpedAndTwisted l) = whenUnrevealed l $ modifySelf l [Blocked]

instance HasAbilities AscendingPathWarpedAndTwisted where
  getAbilities (AscendingPathWarpedAndTwisted a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage AscendingPathWarpedAndTwisted where
  runMessage msg l@(AscendingPathWarpedAndTwisted attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      spendClues iid n
      alteredPaths <- getSetAsideCardsMatching $ CardWithTitle "Altered Path"
      for_ (nonEmpty alteredPaths) (sampleN n >=> traverse_ placeLocation_)
      pure l
    _ -> AscendingPathWarpedAndTwisted <$> liftRunMessage msg attrs
