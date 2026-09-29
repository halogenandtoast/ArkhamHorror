-- | The four Dead of Night investigator sheets and the cards they start with.
module AH3e.Content.DeadOfNight.Investigators (investigators, cards) where

import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Ids
import AH3e.Types.Skill
import Data.Map.Strict qualified as Map

skills :: Int -> Int -> Int -> Int -> Int -> Map Skill Int
skills lore influence observation strength will =
  Map.fromList
    [ (Lore, lore)
    , (Influence, influence)
    , (Observation, observation)
    , (Strength, strength)
    , (Will, will)
    ]

investigators :: [InvestigatorDef]
investigators =
  [ InvestigatorDef
      { id = "roland-banks"
      , name = "Roland Banks"
      , occupation = "The Fed"
      , expansion = DeadOfNight
      , health = 7
      , sanity = 5
      , focusLimit = Just 2
      , skills = skills 2 2 3 3 3
      , starting =
          [ StartingCard "38-special"
          , StartingMoney 3
          , StartingChoice [[StartingCard "follow-up"], [StartingCard "implacable"]]
          ]
      , roles = []
      , abilityText =
          "Fight for the Truth—As part of an attack action, you may add one to the result of a number of dice equal to the number of clues in your neighborhood."
      }
  , InvestigatorDef
      { id = "skids-otoole"
      , name = "\"Skids\" O'Toole"
      , occupation = "The Ex-Convict"
      , expansion = DeadOfNight
      , health = 6
      , sanity = 6
      , focusLimit = Just 0
      , skills = skills 2 1 3 3 4
      , starting =
          [ StartingCard "on-the-lam"
          , StartingMoney 2
          , StartingChoice [[StartingCard "light-fingers"], [StartingCard "switchblade"]]
          ]
      , roles = []
      , abilityText =
          "Won't Go Back—Once per round, after a test in which you rolled a 1, you may focus one skill of your choice, even if it exceeds your focus limit."
      }
  , InvestigatorDef
      { id = "kate-winthrop"
      , name = "Kate Winthrop"
      , occupation = "The Scientist"
      , expansion = DeadOfNight
      , health = 5
      , sanity = 7
      , focusLimit = Just 3
      , skills = skills 3 2 4 2 2
      , starting =
          [ StartingCard "research-notes"
          , StartingMoney 2
          , StartingChoice [[StartingCard "flux-stabilizer"], [StartingCard "replicable-findings"]]
          ]
      , roles = []
      , abilityText =
          "See the Whole Picture—After you perform a research action, you may focus a number of skills of your choice equal to your test result."
      }
  , InvestigatorDef
      { id = "diana-stanley"
      , name = "Diana Stanley"
      , occupation = "The Redeemed Cultist"
      , expansion = DeadOfNight
      , health = 7
      , sanity = 5
      , focusLimit = Just 2
      , skills = skills 4 2 3 3 1
      , starting =
          [ StartingCard "dark-insight"
          , StartingMoney 2
          , StartingChoice [[StartingCard "call-the-storm"], [StartingCard "stolen-amulet"]]
          ]
      , roles = []
      , abilityText =
          "Forbidden Practices—When you would suffer horror (including direct horror), you may place up to two doom in your space to prevent an equal amount of that horror."
      }
  ]

starting
  :: CardCode -> Text -> AssetType -> [Trait] -> Int -> Maybe Int -> Maybe Int -> Text -> CardDef
starting c n ty traits hands health sanity txt =
  CardDef
    c
    n
    DeadOfNight
    1
    (AssetCard (AssetDef ty StartingPile traits Nothing hands health sanity castHorror txt))
 where
  -- 483.4: every starting spell costs one horror to cast
  castHorror = if ty == Spell then 1 else 0

cards :: [CardDef]
cards =
  [ starting
      "38-special"
      ".38 Special"
      Item
      ["Common", "Weapon"]
      1
      Nothing
      Nothing
      "You get +2 strength as part of an attack action. If the monster you are attacking has a remnant icon, you get +3 strength instead."
  , starting
      "follow-up"
      "Follow Up"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "When you would gain a remnant, you may spawn a clue instead."
  , starting
      "implacable"
      "Implacable"
      Talent
      ["Innate"]
      0
      (Just 2)
      (Just 2)
      "When you would gain a remnant, you may recover one health and one sanity instead."
  , starting
      "on-the-lam"
      "On the Lam"
      Talent
      ["Innate"]
      0
      (Just 2)
      (Just 2)
      "During your turn, you may suffer one horror to disengage and exhaust all non-epic monsters engaged with you; non-epic monsters do not engage you until the end of the round."
  , starting
      "light-fingers"
      "Light Fingers"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "After you perform a gather resources action, you may become WANTED to choose one item in the display and test observation +1. If your test result equals or exceeds that item's value, gain that item and discard WANTED."
  , starting
      "switchblade"
      "Switchblade"
      Item
      ["Common", "Curio", "Weapon"]
      1
      Nothing
      Nothing
      "When you disengage a monster, you may deal one damage to it."
  , starting
      "research-notes"
      "Research Notes"
      Item
      ["Common", "Curio", "Tome"]
      0
      Nothing
      Nothing
      "Once per round, when you would reroll a die, you may add one to the result of that die instead."
  , starting
      "flux-stabilizer"
      "Flux Stabilizer"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "Once per round, when a doom or non-epic monster would be placed in your neighborhood, you may discard it instead."
  , starting
      "replicable-findings"
      "Replicable Findings"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "When you place two or more clues on the scenario sheet as part of a single research action, you may place one additional clue from the clue pool."
  , starting
      "dark-insight"
      "Dark Insight"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "Once per round, while resolving a test, you may add one to the result of a number of dice equal to the amount of doom in your space."
  , starting
      "call-the-storm"
      "Call the Storm"
      Spell
      ["Ritual"]
      0
      Nothing
      Nothing
      "Action: Choose any space and test lore -1. Each monster in that space suffers damage equal to your test result. Then, place one doom in that space."
  , starting
      "stolen-amulet"
      "Stolen Amulet"
      Item
      ["Magical", "Curio"]
      0
      Nothing
      Nothing
      "Once per round, during your turn, you may suffer one direct horror to perform one additional action."
  ]
