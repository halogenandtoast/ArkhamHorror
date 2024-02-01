module Arkham.Act.Cards.SkinGame (
  SkinGame (..),
  skinGame,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Campaign
import Arkham.Matcher
import Arkham.Placement
import Arkham.Trait

newtype SkinGame = SkinGame ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

skinGame :: ActCard SkinGame
skinGame =
  act (2, A) SkinGame Cards.skinGame
    $ Just
    $ GroupClueCost (PerPlayer 2)
    $ LocationWithTitle "VIP Area"

instance RunMessage SkinGame where
  runMessage msg a@(SkinGame attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      completedExtracurricularActivity <- completedScenario "02041"
      lead <- getLead
      peterClover <- genCard Assets.peterClover
      drFrancisMorgan <- genCard Assets.drFrancisMorgan
      cloverClubBarId <- getJustLocationByName "Clover Club Bar"
      vipAreaId <- getJustLocationByName "VIP Area"
      assetId <- getRandom
      pushAll
        $ if completedExtracurricularActivity
          then
            [ CreateAssetAt assetId peterClover (AtLocation cloverClubBarId)
            , FindEncounterCard
                lead
                (toTarget attrs)
                (FromEncounterDeck : allOutOfPlayZones)
                (CardWithType EnemyType <> CardWithTrait Abomination)
            , AdvanceToAct (actDeckId attrs) Acts.fold A (toSource attrs)
            ]
          else
            [ CreateAssetAt assetId drFrancisMorgan (AtLocation vipAreaId)
            , AdvanceToAct (actDeckId attrs) Acts.allIn A (toSource attrs)
            ]
      pure a
    FoundEncounterCard _ target ec | isTarget attrs target -> do
      cloverClubBarId <- getJustLocationByName "Clover Club Bar"
      push $ SpawnEnemyAt (EncounterCard ec) cloverClubBarId
      pure a
    _ -> SkinGame <$> runMessage msg attrs
