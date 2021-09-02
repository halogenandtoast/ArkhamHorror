module Arkham.Types.Agenda.Cards.PastPresentAndFuture
  ( PastPresentAndFuture
  , pastPresentAndFuture
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.CardType
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype PastPresentAndFuture = PastPresentAndFuture AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pastPresentAndFuture :: AgendaCard PastPresentAndFuture
pastPresentAndFuture =
  agenda (2, A) PastPresentAndFuture Cards.pastPresentAndFuture (Static 4)

instance HasAbilities PastPresentAndFuture where
  getAbilities (PastPresentAndFuture x) =
    [ mkAbility x 1 $ ForcedAbility $ MovedBy
        Timing.After
        You
        EncounterCardSource
    ]

instance (HasRecord env, AgendaRunner env) => RunMessage env PastPresentAndFuture where
  runMessage msg a@(PastPresentAndFuture attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      sacrificedToYogSothoth <- getRecordCount SacrificedToYogSothoth
      investigatorIds <- getInvestigatorIds
      a <$ pushAll
        ([ ShuffleEncounterDiscardBackIn
         , DiscardEncounterUntilFirst
           (toSource attrs)
           (CardWithType LocationType)
         ]
        <> [ BeginSkillTest
               iid
               (toSource attrs)
               (InvestigatorTarget iid)
               Nothing
               SkillWillpower
               sacrificedToYogSothoth
           | sacrificedToYogSothoth > 0
           , iid <- investigatorIds
           ]
        <> [NextAgenda aid "02314"]
        )
    RequestedEncounterCard source (Just card) | isSource attrs source -> do
      leadInvestigator <- getLeadInvestigatorId
      a <$ push (InvestigatorDrewEncounterCard leadInvestigator card)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> a
      <$ push (InvestigatorAssignDamage iid source DamageAny n 0)
    _ -> PastPresentAndFuture <$> runMessage msg attrs
