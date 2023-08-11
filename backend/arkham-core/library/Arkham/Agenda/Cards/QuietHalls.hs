module Arkham.Agenda.Cards.QuietHalls (
  QuietHalls (..),
  quietHalls,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Campaign
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Projection

newtype QuietHalls = QuietHalls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietHalls :: AgendaCard QuietHalls
quietHalls = agenda (1, A) QuietHalls Cards.quietHalls (Static 7)

instance RunMessage QuietHalls where
  runMessage msg a@(QuietHalls attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      investigatorIds <- getInvestigatorIds
      completedTheHouseAlwaysWins <- elem "02062" <$> getCompletedScenarios
      messages <- flip mapMaybeM investigatorIds $ \iid -> do
        discardCount <- fieldMap InvestigatorDiscard length iid
        if discardCount >= 5
          then
            pure $
              Just
                ( InvestigatorAssignDamage
                    iid
                    (toSource attrs)
                    DamageAny
                    0
                    (if discardCount >= 10 then 2 else 1)
                )
          else pure Nothing

      let
        continueMessages =
          if completedTheHouseAlwaysWins
            then
              [ AdvanceAgendaDeck agendaDeckId (toSource attrs)
              , AdvanceCurrentAgenda
              ]
            else [AdvanceAgendaDeck agendaDeckId (toSource attrs)]

      pushAll $ messages <> continueMessages
      pure a
    _ -> QuietHalls <$> runMessage msg attrs
