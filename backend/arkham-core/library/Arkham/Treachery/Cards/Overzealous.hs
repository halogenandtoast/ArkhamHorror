module Arkham.Treachery.Cards.Overzealous
  ( overzealous
  , Overzealous(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Message
import Arkham.Target
import Arkham.Treachery.Runner

newtype Overzealous = Overzealous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overzealous :: TreacheryCard Overzealous
overzealous = treachery Overzealous Cards.overzealous

instance RunMessage Overzealous where
  runMessage msg t@(Overzealous attrs) = case msg of
    Revelation _iid source | isSource attrs source ->
      t <$ push (DrawEncounterCards (toTarget attrs) 1)
    RequestedEncounterCards target [card] | isTarget attrs target ->
      withTreacheryOwner
        attrs
        \iid -> t <$ pushAll
          [ CreateEffect
            (toCardCode attrs)
            Nothing
            (toSource attrs)
            (CardIdTarget $ toCardId card)
          , InvestigatorDrewEncounterCard iid card
          ]
    _ -> Overzealous <$> runMessage msg attrs
