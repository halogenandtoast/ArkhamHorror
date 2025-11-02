module Arkham.Location.Cards.GrandBazaarMarbleFountain (grandBazaarMarbleFountain) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement

newtype GrandBazaarMarbleFountain = GrandBazaarMarbleFountain LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandBazaarMarbleFountain :: LocationCard GrandBazaarMarbleFountain
grandBazaarMarbleFountain =
  locationWith
    GrandBazaarMarbleFountain
    Cards.grandBazaarMarbleFountain
    5
    (Static 3)
    connectsToAdjacent

instance HasAbilities GrandBazaarMarbleFountain where
  getAbilities (GrandBazaarMarbleFountain a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (enemyAt a <> EnemyWithAnyClues))
      $ FastAbility' Free [#parley]

instance RunMessage GrandBazaarMarbleFountain where
  runMessage msg l@(GrandBazaarMarbleFountain attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select (enemyAt attrs <> EnemyWithAnyClues)
      chooseTargetM iid enemies \enemy -> do
        moveTokens (attrs.ability 1) enemy iid #clue 1
        place enemy InTheShadows
        resolveConcealed iid enemy
      pure l
    _ -> GrandBazaarMarbleFountain <$> liftRunMessage msg attrs
