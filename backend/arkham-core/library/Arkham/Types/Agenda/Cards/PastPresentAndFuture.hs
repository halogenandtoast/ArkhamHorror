module Arkham.Types.Agenda.Cards.PastPresentAndFuture
  ( PastPresentAndFuture
  , pastPresentAndFuture
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
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype PastPresentAndFuture = PastPresentAndFuture AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pastPresentAndFuture :: PastPresentAndFuture
pastPresentAndFuture = PastPresentAndFuture
  $ baseAttrs "02313" "Past, Present and Future" (Agenda 2 A) (Static 4)

instance HasModifiersFor env PastPresentAndFuture where
  getModifiersFor = noModifiersFor

instance HasActions env PastPresentAndFuture where
  getActions i window (PastPresentAndFuture x) = getActions i window x

instance (HasRecord env, AgendaRunner env) => RunMessage env PastPresentAndFuture where
  runMessage msg a@(PastPresentAndFuture attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      sacrificedToYogSothoth <- getRecordCount SacrificedToYogSothoth
      investigatorIds <- getInvestigatorIds
      a <$ unshiftMessages
        ([ ShuffleEncounterDiscardBackIn
         , DiscardEncounterUntilFirst
           (toSource attrs)
           (CardMatchByType (LocationType, mempty))
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
      a <$ unshiftMessage (InvestigatorDrewEncounterCard leadInvestigator card)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source
      -> a <$ unshiftMessage (InvestigatorAssignDamage iid source DamageAny n 0)
    _ -> PastPresentAndFuture <$> runMessage msg attrs
