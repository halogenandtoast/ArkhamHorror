module Arkham.Agenda.Cards.AllIsOne
  ( AllIsOne(..)
  , allIsOne
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype AllIsOne = AllIsOne AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIsOne :: AgendaCard AllIsOne
allIsOne = agenda (1, A) AllIsOne Cards.allIsOne (Static 4)

instance HasAbilities AllIsOne where
  getAbilities (AllIsOne x) =
    [ mkAbility x 1 $ ForcedAbility $ MovedBy
        Timing.After
        You
        EncounterCardSource
    ]

instance RunMessage AllIsOne where
  runMessage msg a@(AllIsOne attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      a <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      failedToSaveStudents <- getHasRecord
        TheInvestigatorsFailedToSaveTheStudents
      investigatorIds <- getInvestigatorIds
      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , DiscardEncounterUntilFirst
            (toSource attrs)
            (CardWithType LocationType)
          ]
        <> [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
           | failedToSaveStudents
           , iid <- investigatorIds
           ]
        <> [AdvanceAgendaDeck agendaDeckId (toSource attrs)]
      pure a
    RequestedEncounterCard source (Just card) | isSource attrs source -> do
      leadInvestigator <- getLeadInvestigatorId
      push $ InvestigatorDrewEncounterCard leadInvestigator card
      pure a
    _ -> AllIsOne <$> runMessage msg attrs
