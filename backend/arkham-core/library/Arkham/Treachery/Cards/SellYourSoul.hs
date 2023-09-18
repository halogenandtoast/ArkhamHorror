module Arkham.Treachery.Cards.SellYourSoul (
  sellYourSoul,
  SellYourSoul (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SellYourSoul = SellYourSoul TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sellYourSoul :: TreacheryCard SellYourSoul
sellYourSoul = treachery SellYourSoul Cards.sellYourSoul

instance RunMessage SellYourSoul where
  runMessage msg t@(SellYourSoul attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasResources <- fieldMap InvestigatorResources (>= 14) iid
      if hasResources
        then push $ LoseResources iid (toSource attrs) 14
        else push $ DrivenInsane iid
      pure t
    _ -> SellYourSoul <$> runMessage msg attrs
