module Arkham.Location.Cards.SnowGraves (snowGraves, SnowGraves (..)) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Campaigns.EdgeOfTheEarth.Supplies
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype SnowGraves = SnowGraves LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snowGraves :: LocationCard SnowGraves
snowGraves =
  symbolLabel
    $ locationWith SnowGraves Cards.snowGraves 3 (PerPlayer 2)
    $ (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 2) YourLocation)

instance HasAbilities SnowGraves where
  getAbilities (SnowGraves a) =
    extendRevealed1 a
      $ groupLimit PerCampaign
      $ restricted a 1 (can.manipulate.deck You)
      $ ReactionAbility (DiscoveringLastClue #after You $ be a)
      $ ShuffleTopOfScenarioDeckIntoYourDeck 3 TekeliliDeck

instance RunMessage SnowGraves where
  runMessage msg l@(SnowGraves attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      recoverSupply GreenSoapstone
      pure l
    _ -> SnowGraves <$> liftRunMessage msg attrs
