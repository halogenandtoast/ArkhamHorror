module Arkham.Types.Agenda.Cards.AllIsOne
  ( AllIsOne
  , allIsOne
  ) where

import Arkham.Prelude

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardMatcher
import Arkham.Types.Card.CardType
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype AllIsOne = AllIsOne AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIsOne :: AllIsOne
allIsOne = AllIsOne $ baseAttrs "02312" "All is One" (Agenda 1 A) (Static 4)

instance HasModifiersFor env AllIsOne where
  getModifiersFor = noModifiersFor

instance HasActions env AllIsOne where
  getActions i window (AllIsOne x) = getActions i window x

instance (HasRecord env, AgendaRunner env) => RunMessage env AllIsOne where
  runMessage msg a@(AllIsOne attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      failedToSaveStudents <- getHasRecord
        TheInvestigatorsFailedToSaveTheStudents
      investigatorIds <- getInvestigatorIds
      a <$ unshiftMessages
        ([ ShuffleEncounterDiscardBackIn
         , DiscardEncounterUntilFirst
           (toSource attrs)
           (CardMatchByType (LocationType, mempty))
         ]
        <> [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
           | failedToSaveStudents
           , iid <- investigatorIds
           ]
        <> [NextAgenda aid "02313"]
        )
    RequestedEncounterCard source (Just card) | isSource attrs source -> do
      leadInvestigator <- getLeadInvestigatorId
      a <$ unshiftMessage (InvestigatorDrewEncounterCard leadInvestigator card)
    _ -> AllIsOne <$> runMessage msg attrs
