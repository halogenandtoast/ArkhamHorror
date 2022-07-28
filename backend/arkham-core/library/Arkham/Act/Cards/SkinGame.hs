module Arkham.Act.Cards.SkinGame
  ( SkinGame(..)
  , skinGame
  ) where

import Arkham.Prelude

import Arkham.Act.Types
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Campaign
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Trait

newtype SkinGame = SkinGame ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

skinGame :: ActCard SkinGame
skinGame = act
  (2, A)
  SkinGame
  Cards.skinGame
  (Just $ GroupClueCost (PerPlayer 2) (LocationWithTitle "VIP Area"))

instance RunMessage SkinGame where
  runMessage msg a@(SkinGame attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      completedExtracurricularActivity <- elem "02041" <$> getCompletedScenarios
      leadInvestigatorId <- getLeadInvestigatorId
      peterClover <- EncounterCard <$> genEncounterCard Assets.peterClover
      drFrancisMorgan <- PlayerCard <$> genPlayerCard Assets.drFrancisMorgan
      cloverClubBarId <- getJustLocationIdByName "Clover Club Bar"
      vipAreaId <- getJustLocationIdByName "VIP Area"
      a <$ if completedExtracurricularActivity
        then pushAll
          [ CreateAssetAt peterClover (AtLocation cloverClubBarId)
          , FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            (CardWithType EnemyType <> CardWithTrait Abomination)
          , AdvanceToAct (actDeckId attrs) Acts.fold A (toSource attrs)
          ]
        else pushAll
          [ CreateAssetAt drFrancisMorgan (AtLocation vipAreaId)
          , AdvanceToAct (actDeckId attrs) Acts.allIn A (toSource attrs)
          ]
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      cloverClubBarId <- getJustLocationIdByName "Clover Club Bar"
      a <$ push (SpawnEnemyAt (EncounterCard ec) cloverClubBarId)
    _ -> SkinGame <$> runMessage msg attrs
