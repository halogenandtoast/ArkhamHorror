module Arkham.Types.Act.Cards.SaracenicScript
  ( SaracenicScript(..)
  , saracenicScript
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Assets
import Arkham.PlayerCard
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message

newtype SaracenicScript = SaracenicScript ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env)

saracenicScript :: SaracenicScript
saracenicScript = SaracenicScript $ baseAttrs
  "02240"
  "Saracenic Script"
  (Act 1 A)
  (Just
  $ RequiredClues (PerPlayer 2) (Just $ LocationWithTitle "Whateley Ruins")
  )

instance ActionRunner env => HasActions env SaracenicScript where
  getActions i window (SaracenicScript x) = getActions i window x

instance ActRunner env => RunMessage env SaracenicScript where
  runMessage msg a@(SaracenicScript attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ | aid == actId && onSide B attrs -> do
      survived <- getHasRecord DrHenryArmitageSurvivedTheDunwichLegacy
      investigatorIds <- map unInScenarioInvestigatorId
        <$> getSetList @InScenarioInvestigatorId ()
      investigatorEsotericFormulaPairs <- for investigatorIds $ \iid ->
        (iid, ) <$> (PlayerCard <$> genPlayerCard Assets.esotericFormula)
      a <$ pushAll
        ([ TakeControlOfSetAsideAsset iid esotericFormula
         | (iid, esotericFormula) <- investigatorEsotericFormulaPairs
         ]
        <> [ PlaceDoomOnAgenda | not survived ]
        <> [NextAct aid "02241"]
        )
    _ -> SaracenicScript <$> runMessage msg attrs
