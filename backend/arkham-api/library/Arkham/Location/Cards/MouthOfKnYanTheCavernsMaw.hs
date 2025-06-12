module Arkham.Location.Cards.MouthOfKnYanTheCavernsMaw (mouthOfKnYanTheCavernsMaw) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck

newtype MouthOfKnYanTheCavernsMaw = MouthOfKnYanTheCavernsMaw LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mouthOfKnYanTheCavernsMaw :: LocationCard MouthOfKnYanTheCavernsMaw
mouthOfKnYanTheCavernsMaw =
  symbolLabel $ location MouthOfKnYanTheCavernsMaw Cards.mouthOfKnYanTheCavernsMaw 2 (Static 0)

instance HasAbilities MouthOfKnYanTheCavernsMaw where
  getAbilities (MouthOfKnYanTheCavernsMaw a) =
    extendRevealed
      a
      [ withTooltip "Let's make camp and solve this puzzle tomorrow" (locationResignAction a)
      , restricted a 2 (Here <> HasSupply Compass) actionAbility
      ]

instance RunMessage MouthOfKnYanTheCavernsMaw where
  runMessage msg l@(MouthOfKnYanTheCavernsMaw attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      explorationDeck <- getExplorationDeck
      let (viewing, rest) = splitAt 3 explorationDeck
      let cardPairs = map (toSnd (`deleteFirst` viewing)) viewing
      focusCards viewing do
        setExplorationDeck rest
        chooseOneM iid do
          questionLabeled "Place one card on bottom of exploration deck"
          for_ cardPairs \(c, remaining) -> targeting c do
            putCardOnBottomOfDeck iid ExplorationDeck c
            unfocusCards
            focusCards remaining do
              chooseOneAtATimeM iid do
                questionLabeled "Place card on top of exploration deck"
                targets remaining $ putCardOnTopOfDeck iid ExplorationDeck
      pure l
    _ -> MouthOfKnYanTheCavernsMaw <$> liftRunMessage msg attrs
