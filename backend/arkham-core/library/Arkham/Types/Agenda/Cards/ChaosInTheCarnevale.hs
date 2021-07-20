module Arkham.Types.Agenda.Cards.ChaosInTheCarnevale
  ( ChaosInTheCarnevale
  , chaosInTheCarnevale
  ) where

import Arkham.Prelude

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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosInTheCarnevale :: ChaosInTheCarnevale
chaosInTheCarnevale = ChaosInTheCarnevale
  $ baseAttrs "82004" "Chaos in the Carnevale" (Agenda 3 A) (Static 3)

instance HasModifiersFor env ChaosInTheCarnevale

instance HasActions env ChaosInTheCarnevale where
  getActions i window (ChaosInTheCarnevale x) = getActions i window x

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
