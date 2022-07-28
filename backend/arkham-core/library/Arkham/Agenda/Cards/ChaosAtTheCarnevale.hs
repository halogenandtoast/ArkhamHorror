module Arkham.Agenda.Cards.ChaosAtTheCarnevale
  ( ChaosAtTheCarnevale(..)
  , chaosAtTheCarnevale
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Agenda.Attrs
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype ChaosAtTheCarnevale = ChaosAtTheCarnevale AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
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

instance RunMessage ChaosAtTheCarnevale where
  runMessage msg a@(ChaosAtTheCarnevale attrs@AgendaAttrs {..}) = case msg of
    UseCardAbility _ source [Window _ (Window.EnemySpawns eid _)] 1 _
      | isSource attrs source -> a <$ push (PlaceDoom (EnemyTarget eid) 2)
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      investigatorIds <- getInvestigatorIds
      mCnidathquaId <- selectOne $ enemyIs Enemies.cnidathqua
      a <$ case mCnidathquaId of
        Just cnidathquaId ->
          pushAll
            $ [ EnemyAttack
                  iid
                  cnidathquaId
                  (DamageFirst Assets.innocentReveler)
                  RegularAttack
              | iid <- investigatorIds
              ]
            <> [RevertAgenda (toId attrs)]
        Nothing -> pure ()
    _ -> ChaosAtTheCarnevale <$> runMessage msg attrs
