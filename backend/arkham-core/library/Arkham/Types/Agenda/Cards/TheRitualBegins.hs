module Arkham.Types.Agenda.Cards.TheRitualBegins where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Trait

newtype TheRitualBegins = TheRitualBegins AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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
  runMessage msg a@(TheRitualBegins attrs@AgendaAttrs {..}) = case msg of
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
    FailedSkillTest iid _ source _ _ _ | isSource attrs source ->
      a <$ unshiftMessage
        (SearchCollectionForRandom
          iid
          (AgendaSource agendaId)
          (PlayerTreacheryType, Just Madness)
        )
    RequestedPlayerCard iid (AgendaSource aid) mcard | aid == agendaId ->
      case mcard of
        Nothing -> pure a
        Just card -> a <$ unshiftMessages
          [AddToHand iid (PlayerCard card), DrewTreachery iid (PlayerCard card)]
    _ -> TheRitualBegins <$> runMessage msg attrs
