module Arkham.Treachery.Cards.SecretsInTheAttic
  ( secretsInTheAttic
  , SecretsInTheAttic(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SecretsInTheAttic = SecretsInTheAttic TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsInTheAttic :: TreacheryCard SecretsInTheAttic
secretsInTheAttic = treachery SecretsInTheAttic Cards.secretsInTheAttic

instance RunMessage SecretsInTheAttic where
  runMessage msg t@(SecretsInTheAttic attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> SecretsInTheAttic <$> runMessage msg attrs
