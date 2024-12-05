module Arkham.Location.Cards.Backstage (backstage, Backstage (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Backstage = Backstage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstage :: LocationCard Backstage
backstage = location Backstage Cards.backstage 3 (Static 1)

instance HasModifiersFor Backstage where
  getModifiersFor (Backstage attrs) = do
    cards <- select $ InHandOf (investigatorAt attrs) <> basic (CardWithKeyword Hidden)
    modifyEach attrs (map (CardIdTarget . toCardId) cards) [HandSizeCardCount 3]

instance HasAbilities Backstage where
  getAbilities (Backstage attrs) =
    extendRevealed1 attrs $ mkAbility attrs 1 $ forced $ RevealLocation #when Anyone (be attrs)

instance RunMessage Backstage where
  runMessage msg l@(Backstage attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      backstageDoorwayCount <- selectCount $ LocationWithUnrevealedTitle "Backstage Doorway"
      backstageDoorways <-
        zip [backstageDoorwayCount ..]
          . take 2
          <$> (shuffleM =<< select (SetAsideCardMatch "Backstage Doorway"))
      for_ backstageDoorways \(idx, backstageDoorway) -> do
        locationId <- placeLocation backstageDoorway
        push $ SetLocationLabel locationId $ "backstageDoorway" <> tshow (idx + 1)
      pure l
    _ -> Backstage <$> liftRunMessage msg attrs
