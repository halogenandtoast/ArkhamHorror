module Arkham.Act.Cards.AtTheStationInShadowedTalons (
  AtTheStationInShadowedTalons (..),
  atTheStationInShadowedTalons,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype AtTheStationInShadowedTalons = AtTheStationInShadowedTalons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atTheStationInShadowedTalons :: ActCard AtTheStationInShadowedTalons
atTheStationInShadowedTalons =
  act (2, C) AtTheStationInShadowedTalons Cards.atTheStationInShadowedTalons
    $ Just
    $ GroupClueCost (PerPlayer 2)
    $ LocationWithTitle "Arkham Police Station"

instance RunMessage AtTheStationInShadowedTalons where
  runMessage msg a@(AtTheStationInShadowedTalons attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide D attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      pushAll
        [ FindEncounterCard
            leadInvestigatorId
            (toTarget attrs)
            [FromEncounterDeck, FromEncounterDiscard, FromOutOfPlayArea VictoryDisplayZone]
            (cardIs Enemies.huntingNightgaunt)
        , NextAdvanceActStep aid 1
        , AdvanceToAct
            (actDeckId attrs)
            Acts.alejandrosPlight
            C
            (toSource attrs)
        ]
      pure a
    NextAdvanceActStep aid 1 | aid == actId attrs && onSide D attrs -> do
      lead <- getLeadPlayer
      huntingNightgaunts <- select $ enemyIs Enemies.huntingNightgaunt
      farthestHuntingNightGaunts <-
        select
          $ FarthestEnemyFromAll
          $ enemyIs
            Enemies.huntingNightgaunt
      deckCount <- getActDecksInPlayCount
      alejandroVela <- getSetAsideCard Assets.alejandroVela
      assetId <- getRandom
      pushAll
        $ map
          ((`HealAllDamage` toSource attrs) . EnemyTarget)
          huntingNightgaunts
        <> [ chooseOrRunOne
              lead
              [ targetLabel huntingNightgaunt
                $ CreateAssetAt
                  assetId
                  alejandroVela
                  (AttachedToEnemy huntingNightgaunt)
                : [ PlaceDoom (toSource attrs) (EnemyTarget huntingNightgaunt) 1
                  | deckCount <= 2
                  ]
              | huntingNightgaunt <- farthestHuntingNightGaunts
              ]
           ]
      pure a
    FoundEncounterCard _ target card | isTarget attrs target -> do
      locations <- select $ FarthestLocationFromAll Anywhere
      lead <- getLeadPlayer
      push
        $ chooseOrRunOne
          lead
          [ targetLabel location [SpawnEnemyAt (EncounterCard card) location]
          | location <- locations
          ]
      pure a
    _ -> AtTheStationInShadowedTalons <$> runMessage msg attrs
