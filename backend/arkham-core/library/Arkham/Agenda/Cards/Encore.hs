module Arkham.Agenda.Cards.Encore (
  Encore (..),
  encore,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype Encore = Encore AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

encore :: AgendaCard Encore
encore = agenda (2, A) Encore Cards.encore (Static 6)

instance HasAbilities Encore where
  getAbilities (Encore a) =
    [ mkAbility a 1
        $ ForcedAbility
        $ AddedToVictory Timing.After
        $ cardIs
          Cards.royalEmissary
    ]

instance RunMessage Encore where
  runMessage msg a@(Encore attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility _ source 1 _ _
      | isSource attrs source ->
          a
            <$ pushAll
              [ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
              , ResetAgendaDeckToStage 1
              , PlaceDoomOnAgenda
              , PlaceDoomOnAgenda
              , PlaceDoomOnAgenda
              ]
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      iids <- getInvestigatorIds
      a
        <$ pushAll
          ( map
              ( \iid -> InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 100
              )
              iids
          )
    _ -> Encore <$> runMessage msg attrs
