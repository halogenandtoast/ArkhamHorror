module Arkham.Treachery.Cards.GraveLight (
  graveLight,
  GraveLight (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype GraveLight = GraveLight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

graveLight :: TreacheryCard GraveLight
graveLight = treachery GraveLight Cards.graveLight

instance RunMessage GraveLight where
  runMessage msg t@(GraveLight attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> do
      let graveLightSpectral = lookupCard Cards.graveLightSpectral (toCardId attrs)
      pushAll
        [ GainSurge (toSource attrs) (toTarget attrs)
        , RemoveTreachery (toId attrs)
        , ReplaceCard (toCardId attrs) graveLightSpectral
        , ShuffleCardsIntoDeck (EncounterDeckByKey SpectralEncounterDeck) [graveLightSpectral]
        ]
      pure t
    _ -> GraveLight <$> runMessage msg attrs
