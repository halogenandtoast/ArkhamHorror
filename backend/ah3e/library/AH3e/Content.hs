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
  focusLimitFromSpells,
  sharesFocusedSkills,
) where

import AH3e.Content.Allies qualified as Allies
import AH3e.Content.Conditions qualified as Conditions
import AH3e.Content.Core.ApproachOfAzathoth qualified as ApproachOfAzathoth
import AH3e.Content.Core.EchoesOfTheDeep qualified as EchoesOfTheDeep
import AH3e.Content.Core.FeastOfUmordhoth qualified as FeastOfUmordhoth
import AH3e.Content.Core.Investigators qualified as Investigators
import AH3e.Content.Core.NeighborhoodCards qualified as NeighborhoodCards
import AH3e.Content.Core.VeilOfTwilight qualified as VeilOfTwilight
import AH3e.Content.DeadOfNight.Encounters qualified as DeadOfNightEncounters
import AH3e.Content.DeadOfNight.Investigators qualified as DeadOfNightInvestigators
import AH3e.Content.DeadOfNight.ShotsInTheDark qualified as ShotsInTheDark
import AH3e.Content.DeadOfNight.SilenceOfTsathoggua qualified as SilenceOfTsathoggua
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

{- | How much holding this card raises its holder's focus limit.
| Sheets whose focus limit is counted from what they hold.
-}
focusLimitFromSpells :: [InvestigatorId]
focusLimitFromSpells = Investigators.focusLimitFromSpells

-- | Cards that share their holder's focused skills with their space.
sharesFocusedSkills :: [CardCode]
sharesFocusedSkills = Investigators.sharesFocusedSkills

focusLimitBonus :: CardCode -> Int
focusLimitBonus code = Map.findWithDefault 0 code (Map.fromList Special.focusLimitBonuses)

cardDefs :: Map CardCode CardDef
cardDefs =
  Map.fromList
    [ (d.code, d)
    | d <-
        ApproachOfAzathoth.cards
          <> EchoesOfTheDeep.cards
          <> FeastOfUmordhoth.cards
          <> VeilOfTwilight.cards
          <> SilenceOfTsathoggua.cards
          <> ShotsInTheDark.cards
          <> DeadOfNightInvestigators.cards
          <> DeadOfNightEncounters.cards
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
investigatorDefs =
  Map.fromList
    [(d.id, d) | d <- Investigators.investigators <> DeadOfNightInvestigators.investigators]

investigatorDef :: InvestigatorId -> Maybe InvestigatorDef
investigatorDef iid = Map.lookup iid investigatorDefs

scenarioDefs :: Map ScenarioCode ScenarioDef
scenarioDefs =
  Map.fromList
    [ (d.code, d)
    | d <-
        [ ApproachOfAzathoth.scenario
        , EchoesOfTheDeep.scenario
        , FeastOfUmordhoth.scenario
        , VeilOfTwilight.scenario
        , SilenceOfTsathoggua.scenario
        , ShotsInTheDark.scenario
        ]
    ]

scenarioDef :: ScenarioCode -> Maybe ScenarioDef
scenarioDef code = Map.lookup code scenarioDefs
