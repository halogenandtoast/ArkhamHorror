module Arkham.Treachery.Cards.SecretsOfTheBeyond
  ( secretsOfTheBeyond
  , SecretsOfTheBeyond(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SecretsOfTheBeyond = SecretsOfTheBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsOfTheBeyond :: TreacheryCard SecretsOfTheBeyond
secretsOfTheBeyond = treachery SecretsOfTheBeyond Cards.secretsOfTheBeyond

instance RunMessage SecretsOfTheBeyond where
  runMessage msg t@(SecretsOfTheBeyond attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> SecretsOfTheBeyond <$> runMessage msg attrs
