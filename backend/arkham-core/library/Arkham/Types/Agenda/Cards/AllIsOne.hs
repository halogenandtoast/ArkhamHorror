module Arkham.Types.Agenda.Cards.AllIsOne
  ( AllIsOne
  , allIsOne
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardType
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Source

newtype AllIsOne = AllIsOne AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIsOne :: AgendaCard AllIsOne
allIsOne = agenda (1, A) AllIsOne Cards.allIsOne (Static 4)

instance HasModifiersFor env AllIsOne

-- TODO: forced ability
instance HasActions AllIsOne

isEncounterCardSource :: Source -> Bool
isEncounterCardSource = \case
  ActSource _ -> True
  AgendaSource _ -> True
  EnemySource _ -> True
  LocationSource _ -> True
  ScenarioSource _ -> True
  TreacherySource _ -> True
  _ -> False

instance (HasRecord env, AgendaRunner env) => RunMessage env AllIsOne where
  runMessage msg a@(AllIsOne attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      failedToSaveStudents <- getHasRecord
        TheInvestigatorsFailedToSaveTheStudents
      investigatorIds <- getInvestigatorIds
      a <$ pushAll
        ([ ShuffleEncounterDiscardBackIn
         , DiscardEncounterUntilFirst
           (toSource attrs)
           (CardWithType LocationType)
         ]
        <> [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
           | failedToSaveStudents
           , iid <- investigatorIds
           ]
        <> [NextAgenda aid "02313"]
        )
    RequestedEncounterCard source (Just card) | isSource attrs source -> do
      leadInvestigator <- getLeadInvestigatorId
      a <$ push (InvestigatorDrewEncounterCard leadInvestigator card)
    MovedBy iid source | isEncounterCardSource source ->
      a <$ push (InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1)
    _ -> AllIsOne <$> runMessage msg attrs
