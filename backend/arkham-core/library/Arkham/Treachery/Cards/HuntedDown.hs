module Arkham.Treachery.Cards.HuntedDown (
  HuntedDown (..),
  huntedDown,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype HuntedDown = HuntedDown TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntedDown :: TreacheryCard HuntedDown
huntedDown = treachery HuntedDown Cards.huntedDown

instance RunMessage HuntedDown where
  runMessage msg t@(HuntedDown attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      enemiesToMove <- select $ UnengagedEnemy <> EnemyWithTrait Criminal
      if null enemiesToMove
        then push $ gainSurge attrs
        else do
          mDestinationId <- field InvestigatorLocation iid
          for_ mDestinationId $ \destinationId -> do
            messages <- for (setToList enemiesToMove) $ \eid -> do
              mLocationId <- selectOne $ LocationWithEnemy $ EnemyWithId eid
              case mLocationId of
                Nothing -> pure Nothing
                Just locationId -> do
                  closestLocationIds <-
                    selectList
                      $ ClosestPathLocation locationId destinationId
                  case closestLocationIds of
                    [] -> pure Nothing
                    [x] ->
                      pure
                        $ Just
                        $ targetLabel
                          eid
                          ( [EnemyMove eid x | locationId /= x]
                              <> [EnemyAttackIfEngaged eid (Just iid)]
                          )
                    xs ->
                      pure
                        $ Just
                        $ targetLabel
                          eid
                          [ chooseOne
                              iid
                              [targetLabel x [EnemyMove eid x] | x <- xs, x /= locationId]
                          , EnemyAttackIfEngaged eid (Just iid)
                          ]

            unless
              (null enemiesToMove || null (catMaybes messages))
              (push $ chooseOneAtATime iid (catMaybes messages))
      pure t
    _ -> HuntedDown <$> runMessage msg attrs
