module Arkham.Act.Cards.AtTheExhibitTheBrotherhoodsPlot (AtTheExhibitTheBrotherhoodsPlot (..), atTheExhibitTheBrotherhoodsPlot) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype AtTheExhibitTheBrotherhoodsPlot = AtTheExhibitTheBrotherhoodsPlot ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

atTheExhibitTheBrotherhoodsPlot :: ActCard AtTheExhibitTheBrotherhoodsPlot
atTheExhibitTheBrotherhoodsPlot = act (2, A) AtTheExhibitTheBrotherhoodsPlot Cards.atTheExhibitTheBrotherhoodsPlot Nothing

instance RunMessage AtTheExhibitTheBrotherhoodsPlot where
  runMessage msg a@(AtTheExhibitTheBrotherhoodsPlot attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      pushAll
        [ FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            [FromEncounterDeck, FromEncounterDiscard, FromOutOfPlayArea VictoryDisplayZone]
            (cardIs Enemies.brotherhoodCultist)
        , NextAdvanceActStep aid 1
        , AdvanceToAct (actDeckId attrs) Acts.recoverTheRelic A (toSource attrs)
        ]
      pure a
    NextAdvanceActStep aid 1 | aid == actId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      brotherhoodCultists <- selectList $ enemyIs Enemies.brotherhoodCultist
      farthestBrotherhoodCultists <-
        selectList $ FarthestEnemyFromAll $ enemyIs Enemies.brotherhoodCultist
      deckCount <- getActDecksInPlayCount
      relicOfAges <- getSetAsideCard Assets.relicOfAgesADeviceOfSomeSort
      assetId <- getRandom
      pushAll
        $ map ((`HealAllDamage` toSource attrs) . toTarget) brotherhoodCultists
        <> [ chooseOrRunOne
              lead
              [ targetLabel cultist
                $ CreateAssetAt assetId relicOfAges (AttachedToEnemy cultist)
                : [PlaceDoom (toSource attrs) (toTarget cultist) 1 | deckCount <= 2]
              | cultist <- farthestBrotherhoodCultists
              ]
           ]
      pure a
    FoundEncounterCard _ target card | isTarget attrs target -> do
      locations <- selectList $ FarthestLocationFromAll Anywhere
      lead <- getLeadPlayer
      push
        $ chooseOrRunOne lead
        $ targetLabels locations (only . SpawnEnemyAt (EncounterCard card))
      pure a
    _ -> AtTheExhibitTheBrotherhoodsPlot <$> runMessage msg attrs
