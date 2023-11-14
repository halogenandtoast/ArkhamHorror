module Arkham.Treachery.Cards.Overzealous (
  overzealous,
  Overzealous (..),
) where

import Arkham.Prelude

import Arkham.Capability
import Arkham.Card
import Arkham.Classes
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Overzealous = Overzealous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overzealous :: TreacheryCard Overzealous
overzealous = treachery Overzealous Cards.overzealous

instance RunMessage Overzealous where
  runMessage msg t@(Overzealous attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      hasEncounterDeck <- can.target.encounterDeck iid
      pushWhen hasEncounterDeck $ DrawEncounterCards (toTarget attrs) 1
      pure t
    RequestedEncounterCards target [card] | isTarget attrs target ->
      withTreacheryOwner
        attrs
        \iid ->
          t
            <$ pushAll
              [ GainSurge (toSource attrs) (toTarget $ toCardId card)
              , InvestigatorDrewEncounterCard iid card
              ]
    _ -> Overzealous <$> runMessage msg attrs
