module Arkham.Agenda.Cards.ChaosAtTheCarnevale (
  ChaosAtTheCarnevale (..),
  chaosAtTheCarnevale,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ChaosAtTheCarnevale = ChaosAtTheCarnevale AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

chaosAtTheCarnevale :: AgendaCard ChaosAtTheCarnevale
chaosAtTheCarnevale =
  agenda (3, A) ChaosAtTheCarnevale Cards.chaosAtTheCarnevale (Static 3)

instance HasAbilities ChaosAtTheCarnevale where
  getAbilities (ChaosAtTheCarnevale x) =
    [ mkAbility x 1
      $ ForcedAbility
      $ EnemySpawns Timing.After Anywhere
      $ enemyIs Enemies.writhingAppendage
    | onSide A x
    ]

instance RunMessage ChaosAtTheCarnevale where
  runMessage msg a@(ChaosAtTheCarnevale attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility _ source 1 [(windowType -> Window.EnemySpawns eid _)] _
      | isSource attrs source -> a <$ push (PlaceDoom (toAbilitySource attrs 1) (toTarget eid) 2)
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      investigatorIds <- getInvestigatorIds
      cnidathquaId <- selectJust $ enemyIs Enemies.cnidathqua
      pushAll
        $ [ EnemyAttack
            $ enemyAttack cnidathquaId attrs iid
            & damageStrategyL
            .~ DamageFirst Assets.innocentReveler
          | iid <- investigatorIds
          ]
        <> [RevertAgenda (toId attrs)]
      pure a
    _ -> ChaosAtTheCarnevale <$> runMessage msg attrs
