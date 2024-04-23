module Arkham.Agenda.Cards.AllIsOne (AllIsOne (..), allIsOne) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude

newtype AllIsOne = AllIsOne AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIsOne :: AgendaCard AllIsOne
allIsOne = agenda (1, A) AllIsOne Cards.allIsOne (Static 4)

instance HasAbilities AllIsOne where
  getAbilities (AllIsOne x) =
    [mkAbility x 1 $ forced $ MovedBy #after You Matcher.EncounterCardSource]

instance RunMessage AllIsOne where
  runMessage msg a@(AllIsOne attrs@AgendaAttrs {..}) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ assignHorror iid (attrs.ability 1) 1
      pure a
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      failedToSaveStudents <- getHasRecord TheInvestigatorsFailedToSaveTheStudents
      lead <- getLead
      investigatorIds <- getInvestigatorIds
      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , DiscardUntilFirst
              lead
              (toSource attrs)
              Deck.EncounterDeck
              (basic #location)
          ]
        <> [ assignHorror iid attrs 1
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
