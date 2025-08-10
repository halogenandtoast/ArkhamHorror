module Arkham.Treachery.Cards.EndlessDescent (endlessDescent) where

import Arkham.Direction
import Arkham.Helpers.Location
import Arkham.Location.Types (Field (LocationLabel))
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EndlessDescent = EndlessDescent TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessDescent :: TreacheryCard EndlessDescent
endlessDescent = treachery EndlessDescent Cards.endlessDescent

instance RunMessage EndlessDescent where
  runMessage msg t@(EndlessDescent attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      selectSortedBy LocationLabel (LocationWithTitle "Mysterious Stairs") >>= \case
        a : b : rest -> do
          selectEach (investigatorAt a) \investigator -> push . Move =<< move attrs investigator b

          selectEach (enemyAt a <> oneOf [UnengagedEnemy, MassiveEnemy]) \enemy ->
            push . Move =<< move attrs enemy b

          unrevealLocation a
          push $ RemoveAllTokens (toSource attrs) (toTarget attrs)
          push $ RemoveAllAttachments (toSource attrs) (toTarget attrs)

          for_ (withIndex1 $ b : rest <> [a]) \(i, l) -> do
            setLocationLabel l $ "mysteriousStairs" <> tshow i
            locationMoved l

          doStep 1 msg -- we need the state to update so which know which locations are unrevealed
        _ -> error "wrong number of stairs"
      pure t
    DoStep 1 msg'@(Revelation _iid (isSource attrs -> True)) -> do
      unrevealed <- select UnrevealedLocation
      labels <- traverse (field LocationLabel) unrevealed
      shuffled <- shuffle unrevealed
      for_ (zip shuffled labels) (uncurry setLocationLabel)
      doStep 2 msg'
      pure t
    DoStep 2 (Revelation _iid (isSource attrs -> True)) -> do
      locations <- selectSortedBy LocationLabel $ LocationWithTitle "Mysterious Stairs"

      for_ (zip locations (drop 1 locations)) \(l1, l2) -> do
        placedLocationDirection l1 Above l2

      addToVictory attrs
      pure t
    _ -> EndlessDescent <$> liftRunMessage msg attrs
