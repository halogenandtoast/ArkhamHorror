module Arkham.Treachery.Cards.ThriceDamnedCuriosity (
  thriceDamnedCuriosity,
  ThriceDamnedCuriosity (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ThriceDamnedCuriosity = ThriceDamnedCuriosity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

thriceDamnedCuriosity :: TreacheryCard ThriceDamnedCuriosity
thriceDamnedCuriosity =
  treachery ThriceDamnedCuriosity Cards.thriceDamnedCuriosity

instance RunMessage ThriceDamnedCuriosity where
  runMessage msg t@(ThriceDamnedCuriosity attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- fieldMap InvestigatorHand ((`div` 3) . length) iid
      when (n > 0)
        $ push
        $ InvestigatorAssignDamage
          iid
          (toSource attrs)
          DamageAny
          n
          0
      pure t
    _ -> ThriceDamnedCuriosity <$> runMessage msg attrs
