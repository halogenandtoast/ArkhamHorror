module Arkham.Agenda.Cards.PastPresentAndFuture (
  PastPresentAndFuture (..),
  pastPresentAndFuture,
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

newtype PastPresentAndFuture = PastPresentAndFuture AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pastPresentAndFuture :: AgendaCard PastPresentAndFuture
pastPresentAndFuture =
  agenda (2, A) PastPresentAndFuture Cards.pastPresentAndFuture (Static 4)

instance HasAbilities PastPresentAndFuture where
  getAbilities (PastPresentAndFuture x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ MovedBy
          Timing.After
          You
          Matcher.EncounterCardSource
    ]

instance RunMessage PastPresentAndFuture where
  runMessage msg a@(PastPresentAndFuture attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      a <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      sacrificedToYogSothoth <- getRecordCount SacrificedToYogSothoth
      investigatorIds <- getInvestigators
      lead <- getLead
      sid <- getRandom
      pushAll
        $ [ ShuffleEncounterDiscardBackIn
          , DiscardUntilFirst lead (toSource attrs) Deck.EncounterDeck (basic $ CardWithType LocationType)
          ]
        <> [ beginSkillTest sid iid attrs iid #willpower (RecordedCount SacrificedToYogSothoth)
           | sacrificedToYogSothoth > 0
           , iid <- investigatorIds
           ]
        <> [advanceAgendaDeck attrs]
      pure a
    RequestedEncounterCard source _ (Just card) | isSource attrs source -> do
      lead <- getLead
      a <$ push (InvestigatorDrewEncounterCard lead card)
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ n | isSource attrs source -> do
      a <$ push (InvestigatorAssignDamage iid source DamageAny n 0)
    _ -> PastPresentAndFuture <$> runMessage msg attrs
