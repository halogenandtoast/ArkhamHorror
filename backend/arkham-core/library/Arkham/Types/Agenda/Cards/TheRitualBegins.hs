module Arkham.Types.Agenda.Cards.TheRitualBegins where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait

newtype TheRitualBegins = TheRitualBegins AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRitualBegins :: AgendaCard TheRitualBegins
theRitualBegins =
  agenda (2, A) TheRitualBegins Cards.theRitualBegins (Static 5)

instance HasModifiersFor env TheRitualBegins where
  getModifiersFor _ (EnemyTarget _) (TheRitualBegins attrs)
    | agendaSequence attrs == Agenda 2 A = pure
    $ toModifiers attrs [EnemyFight 1, EnemyEvade 1]
  getModifiersFor _ _ _ = pure []

instance AgendaRunner env => RunMessage env TheRitualBegins where
  runMessage msg a@(TheRitualBegins attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      iids <- getInvestigatorIds
      a <$ pushAll
        ([ BeginSkillTest
             iid
             (toSource attrs)
             (InvestigatorTarget iid)
             Nothing
             SkillWillpower
             6
         | iid <- iids
         ]
        <> [NextAgenda agendaId "01145"]
        )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push
        (SearchCollectionForRandom iid source
        $ CardWithType PlayerTreacheryType
        <> CardWithTrait Madness
        )
    RequestedPlayerCard iid source mcard | isSource attrs source ->
      case mcard of
        Nothing -> pure a
        Just card -> a <$ push (AddToHand iid (PlayerCard card))
    _ -> TheRitualBegins <$> runMessage msg attrs
