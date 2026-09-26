module AH3e.Content (
  cardDefs,
  cardDef,
  investigatorDefs,
  investigatorDef,
  scenarioDefs,
  scenarioDef,
  ScenarioInfo (..),
  scenarioCatalog,
  focusLimitBonus,
) where

import AH3e.Content.Allies qualified as Allies
import AH3e.Content.Conditions qualified as Conditions
import AH3e.Content.Core.ApproachOfAzathoth qualified as ApproachOfAzathoth
import AH3e.Content.Core.FeastOfUmordhoth qualified as FeastOfUmordhoth
import AH3e.Content.Core.Investigators qualified as Investigators
import AH3e.Content.Core.NeighborhoodCards qualified as NeighborhoodCards
import AH3e.Content.Headlines qualified as Headlines
import AH3e.Content.Items qualified as Items
import AH3e.Content.Monsters qualified as Monsters
import AH3e.Content.Scenarios
import AH3e.Content.Special qualified as Special
import AH3e.Content.Spells qualified as Spells
import AH3e.Content.StreetCards qualified as StreetCards
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Ids
import Data.Map.Strict qualified as Map

-- | How much holding this card raises its holder's focus limit.
focusLimitBonus :: CardCode -> Int
focusLimitBonus code = Map.findWithDefault 0 code (Map.fromList Special.focusLimitBonuses)

cardDefs :: Map CardCode CardDef
cardDefs =
  Map.fromList
    [ (d.code, d)
    | d <-
        ApproachOfAzathoth.cards
          <> FeastOfUmordhoth.cards
          <> Investigators.cards
          <> NeighborhoodCards.cards
          <> StreetCards.cards
          <> Items.cards
          <> Spells.cards
          <> Allies.cards
          <> Special.cards
          <> Monsters.cards
          <> Headlines.cards
          <> Conditions.cards
    ]

cardDef :: CardCode -> Maybe CardDef
cardDef code = Map.lookup code cardDefs

investigatorDefs :: Map InvestigatorId InvestigatorDef
investigatorDefs = Map.fromList [(d.id, d) | d <- Investigators.investigators]

investigatorDef :: InvestigatorId -> Maybe InvestigatorDef
investigatorDef iid = Map.lookup iid investigatorDefs

scenarioDefs :: Map ScenarioCode ScenarioDef
scenarioDefs =
  Map.fromList [(d.code, d) | d <- [ApproachOfAzathoth.scenario, FeastOfUmordhoth.scenario]]

scenarioDef :: ScenarioCode -> Maybe ScenarioDef
scenarioDef code = Map.lookup code scenarioDefs
