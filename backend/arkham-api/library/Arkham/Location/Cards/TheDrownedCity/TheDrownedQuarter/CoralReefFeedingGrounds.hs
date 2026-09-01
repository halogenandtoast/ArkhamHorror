module Arkham.Location.Cards.TheDrownedCity.TheDrownedQuarter.CoralReefFeedingGrounds (coralReefFeedingGrounds) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (increaseThisFloodLevel)
import Arkham.Deck qualified as Deck
import Arkham.Enemy.CardDefs.TheDrownedCity.TheDrownedQuarter qualified as Enemies
import Arkham.Location.CardDefs.TheDrownedCity.TheDrownedQuarter qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CoralReefFeedingGrounds = CoralReefFeedingGrounds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coralReefFeedingGrounds :: LocationCard CoralReefFeedingGrounds
coralReefFeedingGrounds = location CoralReefFeedingGrounds Cards.coralReefFeedingGrounds 4 (PerPlayer 3)

instance HasAbilities CoralReefFeedingGrounds where
  getAbilities (CoralReefFeedingGrounds a) =
    withBaseAbilities a
      $ if a.revealed
        then
          [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
          , restricted a 2 Here $ freeReaction $ DiscoverClues #after You (be a) (atLeast 1)
          ]
        else [mkAbility a 3 $ forced $ Enters #when You (be a)]

instance RunMessage CoralReefFeedingGrounds where
  runMessage msg l@(CoralReefFeedingGrounds attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardUntilFirst iid (attrs.ability 1) Deck.EncounterDeck #enemy
      pure l
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      spawnEnemyAt_ ec attrs
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      underseaParasite <- getSetAsideCard Enemies.underseaParasite
      spawnEnemyAt_ underseaParasite attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      increaseThisFloodLevel attrs
      floodable <- select $ connectedTo (be attrs) <> CanHaveFloodLevelIncreased
      chooseTargetM iid floodable increaseThisFloodLevel
      pure l
    _ -> CoralReefFeedingGrounds <$> liftRunMessage msg attrs
