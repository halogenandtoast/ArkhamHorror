module Arkham.Treachery.Cards.EndlessDescent (endlessDescent, EndlessDescent (..)) where

import Arkham.Classes
import Arkham.Direction
import Arkham.Location.Types (Field (LocationLabel))
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.Prelude
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype EndlessDescent = EndlessDescent TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessDescent :: TreacheryCard EndlessDescent
endlessDescent = treachery EndlessDescent Cards.endlessDescent

instance RunMessage EndlessDescent where
  runMessage msg t@(EndlessDescent attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      locations <- selectSortedBy LocationLabel (LocationWithTitle "Mysterious Stairs")
      case locations of
        a : b : rest -> do
          enemies <- select $ enemyAt a <> oneOf [UnengagedEnemy, MassiveEnemy]
          investigators <- select $ investigatorAt a

          for_ investigators $ \investigator -> do
            push $ Move $ move attrs investigator b

          for_ enemies $ \enemy -> do
            push $ Move $ move attrs enemy b

          pushAll
            [ UnrevealLocation a
            , RemoveAllTokens (toSource attrs) (toTarget attrs)
            , RemoveAllAttachments (toSource attrs) (toTarget attrs)
            ]

          for_ (withIndex1 $ b : rest <> [a]) $ \(i, l) -> do
            pushAll [SetLocationLabel l $ "mysteriousStairs" <> tshow i, LocationMoved l]

          push $ DoStep 1 msg -- we need the state to update so which know which locations are unrevealed
        _ -> error "wrong number of stairs"
      pure t
    DoStep 1 msg'@(Revelation _iid (isSource attrs -> True)) -> runQueueT $ do
      unrevealed <- select UnrevealedLocation
      labels <- traverse (field LocationLabel) unrevealed
      shuffled <- shuffleM unrevealed

      for_ (zip shuffled labels) $ \(location, label) -> do
        push $ SetLocationLabel location label

      push $ DoStep 2 msg'
      pure t
    DoStep 2 (Revelation _iid (isSource attrs -> True)) -> runQueueT $ do
      locations <- selectSortedBy LocationLabel $ LocationWithTitle "Mysterious Stairs"

      for_ (zip locations (drop 1 locations)) $ \(l1, l2) -> do
        push $ PlacedLocationDirection l1 Above l2

      push $ AddToVictory (toTarget attrs)
      pure t
    _ -> EndlessDescent <$> liftRunMessage msg attrs
