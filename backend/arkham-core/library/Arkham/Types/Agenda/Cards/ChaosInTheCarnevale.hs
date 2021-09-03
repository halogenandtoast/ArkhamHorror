module Arkham.Types.Agenda.Cards.ChaosInTheCarnevale
  ( ChaosInTheCarnevale
  , chaosInTheCarnevale
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype ChaosInTheCarnevale = ChaosInTheCarnevale AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosInTheCarnevale :: AgendaCard ChaosInTheCarnevale
chaosInTheCarnevale =
  agenda (3, A) ChaosInTheCarnevale Cards.chaosInTheCarnevale (Static 3)

instance HasAbilities ChaosInTheCarnevale where
  getAbilities (ChaosInTheCarnevale x) =
    [ mkAbility x 1
        $ ForcedAbility
        $ EnemySpawns Timing.After Anywhere
        $ enemyIs Enemies.writhingAppendage
    | onSide A x
    ]

instance AgendaRunner env => RunMessage env ChaosInTheCarnevale where
  runMessage msg a@(ChaosInTheCarnevale attrs@AgendaAttrs {..}) = case msg of
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
    _ -> ChaosInTheCarnevale <$> runMessage msg attrs
