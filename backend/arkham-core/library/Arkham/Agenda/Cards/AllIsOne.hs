module Arkham.Agenda.Cards.AllIsOne (
  AllIsOne (..),
  allIsOne,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing

newtype AllIsOne = AllIsOne AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

allIsOne :: AgendaCard AllIsOne
allIsOne = agenda (1, A) AllIsOne Cards.allIsOne (Static 4)

instance HasAbilities AllIsOne where
  getAbilities (AllIsOne x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ MovedBy
          Timing.After
          You
          Matcher.EncounterCardSource
    ]

instance RunMessage AllIsOne where
  runMessage msg a@(AllIsOne attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ InvestigatorAssignDamage iid source DamageAny 0 1
      pure a
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      failedToSaveStudents <-
        getHasRecord
          TheInvestigatorsFailedToSaveTheStudents
      lead <- getLead
      investigatorIds <- getInvestigatorIds
      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , DiscardUntilFirst
              lead
              (toSource attrs)
              Deck.EncounterDeck
              (BasicCardMatch $ CardWithType LocationType)
          ]
        <> [ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
           | failedToSaveStudents
           , iid <- investigatorIds
           ]
        <> [advanceAgendaDeck attrs]
      pure a
    RequestedEncounterCard source _ (Just card) | isSource attrs source -> do
      leadInvestigator <- getLeadInvestigatorId
      push $ InvestigatorDrewEncounterCard leadInvestigator card
      pure a
    _ -> AllIsOne <$> runMessage msg attrs
