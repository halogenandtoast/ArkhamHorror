module Arkham.Treachery.Cards.ShadowOfAtlachNacha
  ( shadowOfAtlachNacha
  , ShadowOfAtlachNacha(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ShadowOfAtlachNacha = ShadowOfAtlachNacha TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowOfAtlachNacha :: TreacheryCard ShadowOfAtlachNacha
shadowOfAtlachNacha = treachery ShadowOfAtlachNacha Cards.shadowOfAtlachNacha

instance RunMessage ShadowOfAtlachNacha where
  runMessage msg t@(ShadowOfAtlachNacha attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ShadowOfAtlachNacha <$> runMessage msg attrs
