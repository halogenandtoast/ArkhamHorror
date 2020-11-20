{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Treachery.Cards.UmordhothsHunger where

import Arkham.Import

import Arkham.Types.Game.Helpers
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype UmordhothsHunger = UmordhothsHunger Attrs
  deriving newtype (Show, ToJSON, FromJSON)

umordhothsHunger :: TreacheryId -> a -> UmordhothsHunger
umordhothsHunger uuid _ = UmordhothsHunger $ baseAttrs uuid "50037"

instance HasModifiersFor env UmordhothsHunger where
  getModifiersFor = noModifiersFor

instance HasActions env UmordhothsHunger where
  getActions i window (UmordhothsHunger attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env UmordhothsHunger where
  runMessage msg (UmordhothsHunger attrs@Attrs {..}) = case msg of
    Revelation _ source | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      msgs <- for investigatorIds $ \iid -> do
        handCount <- unCardCount <$> getCount iid
        pure $ if handCount == 0
          then InvestigatorKilled iid
          else RandomDiscard iid
      enemyIds <- getSetList @EnemyId ()
      unshiftMessages
        $ msgs
        <> [ HealDamage (EnemyTarget eid) 1 | eid <- enemyIds ]
      UmordhothsHunger <$> runMessage msg (attrs & resolved .~ True)
    _ -> UmordhothsHunger <$> runMessage msg attrs
