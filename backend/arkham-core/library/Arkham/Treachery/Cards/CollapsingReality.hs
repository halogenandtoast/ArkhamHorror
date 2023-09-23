module Arkham.Treachery.Cards.CollapsingReality (
  collapsingReality,
  CollapsingReality (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CollapsingReality = CollapsingReality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collapsingReality :: TreacheryCard CollapsingReality
collapsingReality = treachery CollapsingReality Cards.collapsingReality

instance RunMessage CollapsingReality where
  runMessage msg t@(CollapsingReality attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      let other = InvestigatorAssignDamage iid source DamageAny 2 0
      case mlid of
        Nothing -> push other
        Just lid -> do
          isExtradimensional <-
            fieldP
              LocationTraits
              (member Extradimensional)
              lid
          pushAll
            $ if isExtradimensional
              then
                [ Discard (toSource attrs) (toTarget lid)
                , InvestigatorAssignDamage iid source DamageAny 1 0
                ]
              else [other]
      pure t
    _ -> CollapsingReality <$> runMessage msg attrs
