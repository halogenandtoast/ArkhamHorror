module Arkham.Agenda.Cards.WhatsGoingOn
  ( WhatsGoingOn(..)
  , whatsGoingOn
  ) where

import Arkham.Prelude

import Arkham.Agenda.Types
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message

newtype WhatsGoingOn = WhatsGoingOn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatsGoingOn :: AgendaCard WhatsGoingOn
whatsGoingOn = agenda (1, A) WhatsGoingOn Cards.whatsGoingOn (Static 3)

instance RunMessage WhatsGoingOn where
  runMessage msg a@(WhatsGoingOn attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iid <- getLeadInvestigatorId
      -- The lead investigator can choose the first option even if one of the
      -- investigators has no cards in hand (but at least one does).
      canChooseDiscardOption <- selectAny
        (HandWith $ LengthIs $ GreaterThan $ Static 0)
      pushAll
        [ chooseOne iid
        $ Label
            "The lead investigator takes 2 horror"
            [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2]
        : [ Label
              "Each investigator discards 1 card at random from his or her hand"
              [AllRandomDiscard]
          | canChooseDiscardOption
          ]
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> WhatsGoingOn <$> runMessage msg attrs
