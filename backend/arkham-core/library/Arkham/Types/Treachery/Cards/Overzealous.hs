module Arkham.Types.Treachery.Cards.Overzealous
  ( overzealous
  , Overzealous(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Overzealous = Overzealous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overzealous :: TreacheryCard Overzealous
overzealous = treachery Overzealous Cards.overzealous

instance TreacheryRunner env => RunMessage env Overzealous where
  runMessage msg t@(Overzealous attrs) = case msg of
    Revelation _iid source | isSource attrs source ->
      t <$ pushAll
        [DrawEncounterCards (toTarget attrs) 1, Discard $ toTarget attrs]
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
