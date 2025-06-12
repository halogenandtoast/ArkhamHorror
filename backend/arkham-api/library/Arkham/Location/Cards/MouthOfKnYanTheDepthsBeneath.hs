module Arkham.Location.Cards.MouthOfKnYanTheDepthsBeneath (mouthOfKnYanTheDepthsBeneath) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenario.Deck

newtype MouthOfKnYanTheDepthsBeneath = MouthOfKnYanTheDepthsBeneath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mouthOfKnYanTheDepthsBeneath :: LocationCard MouthOfKnYanTheDepthsBeneath
mouthOfKnYanTheDepthsBeneath =
  symbolLabel
    $ location MouthOfKnYanTheDepthsBeneath Cards.mouthOfKnYanTheDepthsBeneath 2 (PerPlayer 1)

instance HasAbilities MouthOfKnYanTheDepthsBeneath where
  getAbilities (MouthOfKnYanTheDepthsBeneath attrs) =
    extendRevealed1 attrs $ restricted attrs 1 (Here <> HasSupply Map) actionAbility

instance RunMessage MouthOfKnYanTheDepthsBeneath where
  runMessage msg l@(MouthOfKnYanTheDepthsBeneath attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      explorationDeck <- getExplorationDeck
      let (viewing, rest) = splitAt 2 explorationDeck
      let (treacheries, other) = partition (`cardMatch` card_ #treachery) viewing
      focusCards viewing $ continue iid do
        setExplorationDeck $ other <> rest
        shuffleDeck ExplorationDeck
        addToEncounterDiscard treacheries
      pure l
    _ -> MouthOfKnYanTheDepthsBeneath <$> liftRunMessage msg attrs
