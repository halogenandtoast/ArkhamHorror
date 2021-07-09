module Arkham.Types.Event.Cards.Oops
  ( oops
  , Oops(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
-- import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
-- import Arkham.Types.Id
import Arkham.Types.Message
-- import Arkham.Types.SkillTest
-- import Arkham.Types.Source
import Arkham.Types.Target
-- import Arkham.Types.Window

newtype Oops = Oops EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oops :: EventCard Oops
oops = event Oops Cards.oops

instance HasActions env Oops where
-- instance
--   ( HasSet EnemyId env LocationId
--   , HasId LocationId env InvestigatorId
--   , HasSet EnemyId env InvestigatorId
--   , HasSkillTest env
--   )
--   => HasActions env Oops where
--   getActions iid (InHandWindow ownerId (AfterFailSkillTest You n)) (Oops attrs)
--     | n <= 2 && iid == ownerId = do
--       msource <- getSkillTestSource
--       case msource of
--         Just (SkillTestSource _ _ _ (EnemyTarget eid) (Just Fight)) -> do
--           engaged <- (eid `member`) <$> getSet @EnemyId iid
--           location <- getId @LocationId iid
--           locationEnemies <- deleteSet eid <$> getSet @EnemyId location
--           if engaged && notNull locationEnemies
--             then pure
--               [ TargetLabel
--                   (EnemyTarget enemy)
--                   [ InitiatePlayCard
--                       iid
--                       (toCardId attrs)
--                       (Just $ EnemyTarget enemy)
--                       False
--                   ]
--               | enemy <- setToList locationEnemies
--               ]
--             else pure []
--         _ -> pure []
  getActions iid window (Oops attrs) = getActions iid window attrs

instance HasModifiersFor env Oops where
  getModifiersFor = noModifiersFor

instance RunMessage env Oops where
  runMessage msg e@(Oops attrs) = case msg of
    InvestigatorPlayEvent iid eid (Just (EnemyTarget targetId))
      | eid == toId attrs -> e <$ pushAll
        [ CancelFailedByModifierEffects
        , InvestigatorDamageEnemy iid targetId
        , Discard (toTarget attrs)
        ]
    _ -> Oops <$> runMessage msg attrs
