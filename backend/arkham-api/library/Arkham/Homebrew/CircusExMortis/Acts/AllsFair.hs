module Arkham.Homebrew.CircusExMortis.Acts.AllsFair (allsFair) where

import Arkham.Act.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyAsSelfLocation))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Helpers (bigTopRings)
import Arkham.Matcher
import Arkham.Placement (atLocations)

newtype AllsFair = AllsFair ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

allsFair :: ActCard AllsFair
allsFair = act (1, A) AllsFair Cards.allsFair (groupClueCost $ PerPlayer 4)

instance HasModifiersFor AllsFair where
  getModifiersFor (AllsFair a) = when (onSide A a) do
    modifySelect a (locationIs Locations.circusGatesDoorwayToDoom) [Blank]
    rings <- select bigTopRings
    modifySelect a Anyone (map CannotEnter rings)

instance RunMessage AllsFair where
  runMessage msg a@(AllsFair attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      extraClues <- perPlayer 1
      rings <- select bigTopRings
      for_ rings \lid -> do
        reveal lid
        placeClues attrs lid extraClues
      eid <- createSetAsideEnemy Enemies.sylvesterBlake (atLocations rings)
      updateEnemy eid EnemyAsSelfLocation (Just "sylvesterBlake")
      advanceActDeck attrs
      pure a
    _ -> AllsFair <$> liftRunMessage msg attrs
