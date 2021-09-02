module Arkham.Types.Agenda.Cards.WhatsGoingOn where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message

newtype WhatsGoingOn = WhatsGoingOn AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatsGoingOn :: AgendaCard WhatsGoingOn
whatsGoingOn = agenda (1, A) WhatsGoingOn Cards.whatsGoingOn (Static 3)

instance AgendaRunner env => RunMessage env WhatsGoingOn where
  runMessage msg a@(WhatsGoingOn attrs) = case msg of
    AdvanceAgenda aid
      | aid == toId attrs && agendaSequence attrs == Agenda 1 B -> do
        iid <- getLeadInvestigatorId
        hasDiscardableCards <- notNull
          <$> select (HandWith $ LengthIs $ GreaterThan $ Static 0)
        a <$ pushAll
          [ chooseOne iid
          $ Label
              "The lead investigator takes 2 horror"
              [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2]
          : [ Label
                "Each investigator discards 1 card at random from his or her hand"
                [AllRandomDiscard]
            | hasDiscardableCards
            ]
          , NextAgenda aid "01106"
          ]
    _ -> WhatsGoingOn <$> runMessage msg attrs
