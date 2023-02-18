module Arkham.Act.Cards.AtTheStationInShadowedTalons
  ( AtTheStationInShadowedTalons(..)
  , atTheStationInShadowedTalons
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Target
import Arkham.Zone

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
          [FromEncounterDeck, FromEncounterDiscard, FromVictoryDisplay]
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
      leadInvestigatorId <- getLeadInvestigatorId
      huntingNightgaunts <- selectList $ enemyIs Enemies.huntingNightgaunt
      farthestHuntingNightGaunts <- selectList $ FarthestEnemyFromAll $ enemyIs
        Enemies.huntingNightgaunt
      deckCount <- getActDecksInPlayCount
      alejandroVela <- getSetAsideCard Assets.alejandroVela
      pushAll
        $ map
            ((`HealAllDamage` toSource attrs) . EnemyTarget)
            huntingNightgaunts
        <> [ chooseOrRunOne
               leadInvestigatorId
               [ targetLabel huntingNightgaunt
                 $ CreateAssetAt
                     alejandroVela
                     (AttachedToEnemy huntingNightgaunt)
                 : [ PlaceDoom (EnemyTarget huntingNightgaunt) 1
                   | deckCount <= 2
                   ]
               | huntingNightgaunt <- farthestHuntingNightGaunts
               ]
           ]
      pure a
    FoundEncounterCard _ target card | isTarget attrs target -> do
      locations <- selectList $ FarthestLocationFromAll Anywhere
      leadInvestigatorId <- getLeadInvestigatorId
      push $ chooseOrRunOne
        leadInvestigatorId
        [ targetLabel location [SpawnEnemyAt (EncounterCard card) location]
        | location <- locations
        ]
      pure a
    _ -> AtTheStationInShadowedTalons <$> runMessage msg attrs
