module Arkham.Treachery.Cards.GraveLightSpectral (
  graveLightSpectral,
  GraveLightSpectral (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype GraveLightSpectral = GraveLightSpectral TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveLightSpectral :: TreacheryCard GraveLightSpectral
graveLightSpectral = treachery GraveLightSpectral Cards.graveLightSpectral

instance RunMessage GraveLightSpectral where
  runMessage msg t@(GraveLightSpectral attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      let graveLight = lookupCard Cards.graveLight (toCardId attrs)
      pushAll
        [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 2 0
        , RemoveTreachery (toId attrs)
        , ReplaceCard (toCardId attrs) graveLight
        , ShuffleCardsIntoDeck EncounterDeck [graveLight]
        ]
      pure t
    _ -> GraveLightSpectral <$> runMessage msg attrs
