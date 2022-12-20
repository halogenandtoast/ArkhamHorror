module Arkham.Treachery.Cards.UmordhothsHunger where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype UmordhothsHunger = UmordhothsHunger TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhothsHunger :: TreacheryCard UmordhothsHunger
umordhothsHunger = treachery UmordhothsHunger Cards.umordhothsHunger

instance RunMessage UmordhothsHunger where
  runMessage msg t@(UmordhothsHunger attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      investigatorIds <- getInvestigatorIds
      msgs <- for investigatorIds $ \iid -> do
        handCount <- fieldMap InvestigatorHand length iid
        pure $ if handCount == 0
          then InvestigatorKilled source iid
          else RandomDiscard iid
      targets <- selectListMap EnemyTarget AnyEnemy
      t <$ pushAll (msgs <> [ HealDamage target (toSource attrs) 1 | target <- targets ])
    _ -> UmordhothsHunger <$> runMessage msg attrs
