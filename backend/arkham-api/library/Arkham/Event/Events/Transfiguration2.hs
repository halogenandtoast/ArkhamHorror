module Arkham.Event.Events.Transfiguration2 (transfiguration2) where

import Arkham.CampaignLogKey
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Log
import Arkham.Investigator.Cards (allInvestigatorCards)
import Arkham.Investigator.Types (InvestigatorForm (..))
import Arkham.Matcher
import Arkham.Name

newtype Transfiguration2 = Transfiguration2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

transfiguration2 :: EventCard Transfiguration2
transfiguration2 = event Transfiguration2 Cards.transfiguration2

instance RunMessage Transfiguration2 where
  runMessage msg e@(Transfiguration2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      inPlay <- selectMap unInvestigatorId $ IncludeEliminated Anyone
      unavailable <-
        ((inPlay <>) .)
          . (<>)
          <$> getSomeRecordSet KilledInvestigators
          <*> getSomeRecordSet DrivenInsaneInvestigators
      let
        allCards = toList allInvestigatorCards
        names = map toName $ filter (any (`elem` unavailable) . (.cardCodes)) allCards
        canBeChosen card = toName card `notElem` names
        cards = filter canBeChosen allCards

      chooseOneM iid do
        for_ cards \card -> do
          cardLabeled card $ push $ SetInvestigatorForm iid $ TransfiguredForm card.cardCode
      pure e
    _ -> Transfiguration2 <$> liftRunMessage msg attrs
