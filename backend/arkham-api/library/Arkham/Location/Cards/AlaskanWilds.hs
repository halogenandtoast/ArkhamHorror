module Arkham.Location.Cards.AlaskanWilds (alaskanWilds) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardDef
import Arkham.Cost
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype AlaskanWilds = AlaskanWilds LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alaskanWilds :: LocationCard AlaskanWilds
alaskanWilds = location AlaskanWilds Cards.alaskanWilds 3 (PerPlayer 2)

mirageCards :: [CardDef]
mirageCards = [Cards.memoryOfAHuntGoneAwry]

instance HasModifiersFor AlaskanWilds where
  getModifiersFor (AlaskanWilds a) = do
    modifySelfWhenM
      a
      ( selectAny
          $ mapOneOf assetIs [Assets.eliyahAshevakDogHandler, Assets.eliyahAshevakDogHandlerResolute]
          <> at_ (be a)
      )
      [ShroudModifier (-2)]
    modifySelf
      a
      [AdditionalCostToEnter (OrCost [ShuffleTopOfScenarioDeckIntoYourDeck 1 TekeliliDeck, ActionCost 1])]
    clearedOfMirages a mirageCards

instance HasAbilities AlaskanWilds where
  getAbilities (AlaskanWilds a) = extendRevealed a [mirage a 2 mirageCards]

instance RunMessage AlaskanWilds where
  runMessage msg (AlaskanWilds attrs) = runQueueT $ case msg of
    _ -> AlaskanWilds <$> mirageRunner Stories.alaskanWilds mirageCards 2 msg attrs
