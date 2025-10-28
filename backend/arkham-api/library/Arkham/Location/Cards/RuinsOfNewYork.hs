module Arkham.Location.Cards.RuinsOfNewYork (ruinsOfNewYork) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Query
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection

newtype RuinsOfNewYork = RuinsOfNewYork LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfNewYork :: LocationCard RuinsOfNewYork
ruinsOfNewYork = location RuinsOfNewYork Cards.ruinsOfNewYork 1 (Static 3)

instance HasAbilities RuinsOfNewYork where
  getAbilities (RuinsOfNewYork a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ PutLocationIntoPlay #after Anyone (be a)

instance RunMessage RuinsOfNewYork where
  runMessage msg l@(RuinsOfNewYork attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      playerCount <- getPlayerCount
      deck <- fieldMap InvestigatorDeck unDeck =<< getLead
      let (cards, _) = splitAt (if playerCount >= 3 then 2 else 1) deck
      let polyps = map (\card -> PlayerCard $ card {pcCardCode = "xpolyp"}) cards
      pushAll $ map (RemovePlayerCardFromGame False . PlayerCard) cards
      for_ polyps (`createEnemy_` AtLocation attrs.id)
      pure l
    _ -> RuinsOfNewYork <$> liftRunMessage msg attrs
