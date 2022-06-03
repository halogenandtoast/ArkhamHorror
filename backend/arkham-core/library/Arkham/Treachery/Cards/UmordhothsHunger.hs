module Arkham.Treachery.Cards.UmordhothsHunger where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Message
import Arkham.Query
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype UmordhothsHunger = UmordhothsHunger TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhothsHunger :: TreacheryCard UmordhothsHunger
umordhothsHunger = treachery UmordhothsHunger Cards.umordhothsHunger

instance TreacheryRunner env => RunMessage UmordhothsHunger where
  runMessage msg t@(UmordhothsHunger attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      msgs <- for investigatorIds $ \iid -> do
        handCount <- unCardCount <$> getCount iid
        pure $ if handCount == 0
          then InvestigatorKilled source iid
          else RandomDiscard iid
      enemyIds <- getSetList @EnemyId ()
      t <$ pushAll
        (msgs <> [ HealDamage (EnemyTarget eid) 1 | eid <- enemyIds ])
    _ -> UmordhothsHunger <$> runMessage msg attrs
