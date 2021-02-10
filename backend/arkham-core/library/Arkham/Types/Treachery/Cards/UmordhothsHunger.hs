module Arkham.Types.Treachery.Cards.UmordhothsHunger where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.TreacheryId


import Arkham.Types.Game.Helpers
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype UmordhothsHunger = UmordhothsHunger TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhothsHunger :: TreacheryId -> a -> UmordhothsHunger
umordhothsHunger uuid _ = UmordhothsHunger $ baseAttrs uuid "50037"

instance HasModifiersFor env UmordhothsHunger where
  getModifiersFor = noModifiersFor

instance HasActions env UmordhothsHunger where
  getActions i window (UmordhothsHunger attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env UmordhothsHunger where
  runMessage msg t@(UmordhothsHunger attrs@TreacheryAttrs {..}) = case msg of
    Revelation _ source | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      msgs <- for investigatorIds $ \iid -> do
        handCount <- unCardCount <$> getCount iid
        pure $ if handCount == 0
          then InvestigatorKilled iid
          else RandomDiscard iid
      enemyIds <- getSetList @EnemyId ()
      t <$ unshiftMessages
        (msgs
        <> [ HealDamage (EnemyTarget eid) 1 | eid <- enemyIds ]
        <> [Discard $ toTarget attrs]
        )
    _ -> UmordhothsHunger <$> runMessage msg attrs
