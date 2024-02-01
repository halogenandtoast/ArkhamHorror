module Arkham.Treachery.Cards.TerrorUnleashed (
  terrorUnleashed,
  TerrorUnleashed (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Location.BreachStatus
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TerrorUnleashed = TerrorUnleashed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

terrorUnleashed :: TreacheryCard TerrorUnleashed
terrorUnleashed = treachery TerrorUnleashed Cards.terrorUnleashed

instance RunMessage TerrorUnleashed where
  runMessage msg t@(TerrorUnleashed attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      mLocation <- selectOne $ locationWithInvestigator iid
      for_ mLocation $ \location -> do
        breaches <- fieldMap LocationBreaches (maybe 0 countBreaches) location
        doom <- field LocationDoom location

        let x = doom + max 1 breaches

        player <- getPlayer iid
        pushAll
          $ (guard (breaches == 0) *> [PlaceBreaches (toTarget location) 1])
          <> replicate x (chooseOne player [assignDamageLabel iid attrs 1, assignHorrorLabel iid attrs 1])
      pure t
    _ -> TerrorUnleashed <$> runMessage msg attrs
