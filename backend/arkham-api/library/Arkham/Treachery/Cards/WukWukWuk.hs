module Arkham.Treachery.Cards.WukWukWuk (wukWukWuk) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WukWukWuk = WukWukWuk TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wukWukWuk :: TreacheryCard WukWukWuk
wukWukWuk = treachery WukWukWuk Cards.wukWukWuk

instance RunMessage WukWukWuk where
  runMessage msg t@(WukWukWuk attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      penguins <- select $ FarthestEnemyFrom iid $ enemyIs Enemies.giantAlbinoPenguin
      if null penguins
        then findAndDrawEncounterCard iid $ cardIs Enemies.giantAlbinoPenguin
        else chooseOrRunOneM iid do
          targets penguins \eid -> do
            chooseOneM iid do
              withLocationOf iid \lid -> do
                whenM (eid <=~> (notAt_ (LocationWithId lid) <> EnemyCanEnter (LocationWithId lid))) do
                  labeled "Move it to your location" $ enemyMoveTo eid lid
              labeled "Place 1 doom on it" $ placeDoom attrs eid 1
      pure t
    _ -> WukWukWuk <$> liftRunMessage msg attrs
