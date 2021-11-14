module Arkham.Types.Agenda.Cards.Encore
  ( Encore
  , encore
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing

newtype Encore = Encore AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encore :: AgendaCard Encore
encore = agenda (2, A) Encore Cards.encore (Static 6)

instance HasAbilities Encore where
  getAbilities (Encore a) =
    [ mkAbility a 1 $ ForcedAbility $ AddedToVictory Timing.After $ cardIs
        Cards.royalEmissary
    ]

instance AgendaRunner env => RunMessage env Encore where
  runMessage msg a@(Encore attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ RemoveAllDoom
      , ResetAgendaDeckToStage 1
      , PlaceDoomOnAgenda
      , PlaceDoomOnAgenda
      , PlaceDoomOnAgenda
      ]
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      iids <- getInvestigatorIds
      a <$ pushAll
        (map
          (\iid -> InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 100
          )
          iids
        )
    _ -> Encore <$> runMessage msg attrs
