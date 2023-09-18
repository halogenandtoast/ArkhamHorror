module Arkham.Treachery.Cards.FinePrint (
  finePrint,
  FinePrint (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Treachery.Runner

newtype FinePrint = FinePrint TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finePrint :: TreacheryCard FinePrint
finePrint = treachery FinePrint Cards.finePrint

instance RunMessage FinePrint where
  runMessage msg t@(FinePrint attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasResources <- fieldMap InvestigatorResources (>= 7) iid
      if hasResources
        then push $ LoseResources iid (toSource attrs) 7
        else do
          case toCard attrs of
            EncounterCard _ -> error "should be player card"
            VengeanceCard _ -> error "should be player card"
            PlayerCard pc -> do
              sellYourSoul <- genPlayerCard Treacheries.sellYourSoul
              pushAll
                [ RemoveCardFromDeckForCampaign iid pc
                , AddCardToDeckForCampaign iid sellYourSoul
                ]
      pure t
    _ -> FinePrint <$> runMessage msg attrs
