module Arkham.Types.Agenda.Cards.ChaosInTheCarnevale
  ( ChaosInTheCarnevale
  , chaosInTheCarnevale
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Enemy.Cards as Enemies
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Helpers
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Message

newtype ChaosInTheCarnevale = ChaosInTheCarnevale AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosInTheCarnevale :: AgendaCard ChaosInTheCarnevale
chaosInTheCarnevale =
  agenda (3, A) ChaosInTheCarnevale Cards.chaosInTheCarnevale (Static 3)

instance HasModifiersFor env ChaosInTheCarnevale
instance HasActions ChaosInTheCarnevale

instance AgendaRunner env => RunMessage env ChaosInTheCarnevale where
  runMessage msg a@(ChaosInTheCarnevale attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 3 B -> do
      investigatorIds <- getInvestigatorIds
      mCnidathquaId <- fmap unStoryEnemyId
        <$> getId (toCardCode Enemies.cnidathqua)
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
