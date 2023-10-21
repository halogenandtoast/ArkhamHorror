module Arkham.Treachery.Cards.IncriminatingEvidence
  ( incriminatingEvidence
  , IncriminatingEvidence(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype IncriminatingEvidence = IncriminatingEvidence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

incriminatingEvidence :: TreacheryCard IncriminatingEvidence
incriminatingEvidence = treachery IncriminatingEvidence Cards.incriminatingEvidence

instance RunMessage IncriminatingEvidence where
  runMessage msg t@(IncriminatingEvidence attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> IncriminatingEvidence <$> runMessage msg attrs
