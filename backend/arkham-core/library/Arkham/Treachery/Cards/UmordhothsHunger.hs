module Arkham.Treachery.Cards.UmordhothsHunger where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field(..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype UmordhothsHunger = UmordhothsHunger TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

umordhothsHunger :: TreacheryCard UmordhothsHunger
umordhothsHunger = treachery UmordhothsHunger Cards.umordhothsHunger

instance RunMessage UmordhothsHunger where
  runMessage msg t@(UmordhothsHunger attrs) = case msg of
    Revelation _ (isSource attrs -> True) -> do
      investigatorIds <- getInvestigatorIds
      msgs <- for investigatorIds $ \iid -> do
        handCount <- fieldMap InvestigatorHand length iid
        pure $ if handCount == 0
          then InvestigatorKilled (toSource attrs) iid
          else toMessage $ randomDiscard iid attrs
      targets <- selectListMap EnemyTarget AnyEnemy
      pushAll
        (msgs <> [ HealDamage target (toSource attrs) 1 | target <- targets ])
      pure t
    _ -> UmordhothsHunger <$> runMessage msg attrs
