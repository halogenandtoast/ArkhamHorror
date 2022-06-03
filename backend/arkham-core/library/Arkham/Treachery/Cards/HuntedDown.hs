module Arkham.Treachery.Cards.HuntedDown
  ( HuntedDown(..)
  , huntedDown
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype HuntedDown = HuntedDown TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntedDown :: TreacheryCard HuntedDown
huntedDown = treachery HuntedDown Cards.huntedDown

instance TreacheryRunner env => RunMessage HuntedDown where
  runMessage msg t@(HuntedDown attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      destinationId <- getId @LocationId iid
      enemiesToMove <- select $ UnengagedEnemy <> EnemyWithTrait Criminal
      if null enemiesToMove
        then t <$ push (Surge iid $ toSource attrs)
        else do
          messages <- for (setToList enemiesToMove) $ \eid -> do
            mLocationId <- selectOne $ LocationWithEnemy $ EnemyWithId  eid
            case mLocationId of
              Nothing -> pure Nothing
              Just locationId -> do
                closestLocationIds <- map unClosestPathLocationId
                  <$> getSetList (locationId, destinationId)
                case closestLocationIds of
                  [] -> pure Nothing
                  [x] -> pure $ Just $ TargetLabel
                    (EnemyTarget eid)
                    ([ EnemyMove eid x | locationId /= x ]
                    <> [EnemyAttackIfEngaged eid (Just iid)]
                    )
                  xs -> pure $ Just $ TargetLabel
                    (EnemyTarget eid)
                    [ chooseOne
                      iid
                      [ EnemyMove eid x | x <- xs, x /= locationId ]
                    , EnemyAttackIfEngaged eid (Just iid)
                    ]

          t <$ unless
            (null enemiesToMove || null (catMaybes messages))
            (push $ chooseOneAtATime iid (catMaybes messages))
    _ -> HuntedDown <$> runMessage msg attrs
