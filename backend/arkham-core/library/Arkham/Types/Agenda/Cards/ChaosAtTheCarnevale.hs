module Arkham.Types.Agenda.Cards.ChaosAtTheCarnevale
  ( ChaosAtTheCarnevale
  , chaosAtTheCarnevale
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype ChaosAtTheCarnevale = ChaosAtTheCarnevale AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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

instance AgendaRunner env => RunMessage env ChaosAtTheCarnevale where
  runMessage msg a@(ChaosAtTheCarnevale attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility _ source [Window _ (Window.EnemySpawns eid _)] 1 _
      | isSource attrs source -> a <$ push (PlaceDoom (EnemyTarget eid) 2)
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      investigatorIds <- getInvestigatorIds
      mCnidathquaId <- selectOne $ enemyIs Enemies.cnidathqua
      a <$ case mCnidathquaId of
        Just cnidathquaId ->
          pushAll
            $ [ EnemyAttack
                  iid
                  cnidathquaId
                  (DamageFirst Assets.innocentReveler)
              | iid <- investigatorIds
              ]
            <> [RevertAgenda (toId attrs)]
        Nothing -> pure ()
    _ -> ChaosAtTheCarnevale <$> runMessage msg attrs
