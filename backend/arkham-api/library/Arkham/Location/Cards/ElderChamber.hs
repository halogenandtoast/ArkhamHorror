module Arkham.Location.Cards.ElderChamber (elderChamber) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card.CardDef
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype ElderChamber = ElderChamber LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elderChamber :: LocationCard ElderChamber
elderChamber = location ElderChamber Cards.elderChamber 1 (PerPlayer 2)

mirageCards :: [CardDef]
mirageCards = [Cards.clutteredDormitory]

instance HasModifiersFor ElderChamber where
  getModifiersFor (ElderChamber a) = clearedOfMirages a mirageCards

instance HasAbilities ElderChamber where
  getAbilities (ElderChamber a) =
    extendRevealed
      a
      [ mirage a 2 mirageCards
      , restricted a 1 (ScenarioDeckWithCard TekeliliDeck)
          $ forced
          $ DiscoverClues #after You (be a) (atLeast 2)
      ]

instance RunMessage ElderChamber where
  runMessage msg l@(ElderChamber attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addTekelili iid =<< getTekelili 1
      pure l
    _ -> ElderChamber <$> mirageRunner Stories.elderChamber mirageCards 2 msg attrs
