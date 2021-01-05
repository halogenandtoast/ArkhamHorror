module Arkham.Types.Agenda.Cards.TheRitualBegins where

import Arkham.Import

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Trait

newtype TheRitualBegins = TheRitualBegins Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theRitualBegins :: TheRitualBegins
theRitualBegins = TheRitualBegins
  $ baseAttrs "01144" "The Ritual Begins" (Agenda 2 A) (Static 5)

instance HasModifiersFor env TheRitualBegins where
  getModifiersFor _ (EnemyTarget _) (TheRitualBegins attrs)
    | agendaSequence attrs == Agenda 2 A = pure
    $ toModifiers attrs [EnemyFight 1, EnemyEvade 1]
  getModifiersFor _ _ _ = pure []

instance HasActions env TheRitualBegins where
  getActions i window (TheRitualBegins x) = getActions i window x

instance (AgendaRunner env) => RunMessage env TheRitualBegins where
  runMessage msg a@(TheRitualBegins attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 A -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      unshiftMessage (chooseOne leadInvestigatorId [AdvanceAgenda agendaId])
      pure
        $ TheRitualBegins
        $ attrs
        & (sequenceL .~ Agenda 2 B)
        & (flippedL .~ True)
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      investigatorIds <- getSetList ()
      a <$ unshiftMessages
        ([ BeginSkillTest
             iid
             (AgendaSource agendaId)
             (InvestigatorTarget iid)
             Nothing
             SkillWillpower
             6
         | iid <- investigatorIds
         ]
        <> [NextAgenda agendaId "01145"]
        )
    FailedSkillTest iid _ source _ _ | isSource attrs source ->
      a <$ unshiftMessage
        (SearchCollectionForRandom
          iid
          (AgendaSource agendaId)
          (PlayerTreacheryType, Just Madness)
        )
    RequestedPlayerCard iid (AgendaSource aid) mcard | aid == agendaId -> do
      case mcard of
        Nothing -> pure a
        Just card -> a <$ unshiftMessages
          [ AddToHand iid (PlayerCard card)
          , DrewPlayerTreachery iid (pcCardCode card) (pcId card)
          ]
    _ -> TheRitualBegins <$> runMessage msg attrs
