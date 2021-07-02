module Arkham.Types.Treachery.Cards.HuntedDown
  ( HuntedDown(..)
  , huntedDown
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype HuntedDown = HuntedDown TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntedDown :: TreacheryCard HuntedDown
huntedDown = treachery HuntedDown Cards.huntedDown

instance HasModifiersFor env HuntedDown where
  getModifiersFor = noModifiersFor

instance HasActions env HuntedDown where
  getActions i window (HuntedDown attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env HuntedDown where
  runMessage msg t@(HuntedDown attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      destinationId <- getId @LocationId iid
      unengagedEnemyIds <- mapSet unUnengagedEnemyId <$> getSet ()
      criminalEnemyIds <- getSet Criminal
      let enemiesToMove = criminalEnemyIds `intersection` unengagedEnemyIds
      if null enemiesToMove
        then
          t <$ unshiftMessages
            [Surge iid $ toSource attrs, Discard $ toTarget attrs]
        else do
          messages <- for (setToList enemiesToMove) $ \eid -> do
            locationId <- getId eid
            closestLocationIds <- map unClosestPathLocationId
              <$> getSetList (locationId, destinationId)
            case closestLocationIds of
              [] -> pure Nothing
              [x] -> pure $ Just $ TargetLabel
                (EnemyTarget eid)
                ([ EnemyMove eid locationId x | locationId /= x ]
                <> [EnemyAttackIfEngaged eid (Just iid)]
                )
              xs -> pure $ Just $ TargetLabel
                (EnemyTarget eid)
                [ chooseOne
                  iid
                  [ EnemyMove eid locationId x | x <- xs, x /= locationId ]
                , EnemyAttackIfEngaged eid (Just iid)
                ]

          t <$ unless
            (null enemiesToMove || null (catMaybes messages))
            (unshiftMessages
              [ chooseOneAtATime iid (catMaybes messages)
              , Discard $ toTarget attrs
              ]
            )
    _ -> HuntedDown <$> runMessage msg attrs
