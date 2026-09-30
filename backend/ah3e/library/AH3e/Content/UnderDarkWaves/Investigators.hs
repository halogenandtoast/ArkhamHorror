-- | The seven Under Dark Waves investigator sheets and the cards they start with.
module AH3e.Content.UnderDarkWaves.Investigators (
  investigators,
  cards,
  focusLimitFromAllies,
  spendableMoneyCards,
  encounterPhaseSkillBonuses,
) where

import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
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

{- | Sheets whose focus limit is counted from the allies they hold rather than
printed, the way Dexter Drake's is counted from his spells.
-}
focusLimitFromAllies :: [InvestigatorId]
focusLimitFromAllies = ["charlie-kane"]

{- | Cards whose money its holder may spend as their own. Read where a price is
worked out, which cannot see behaviours.
-}
spendableMoneyCards :: [CardCode]
spendableMoneyCards = ["calling-in-favors"]

{- | Cards that raise every skill their holder has during the encounter phase.
Read where skills are worked out, which cannot see behaviours either.
-}
encounterPhaseSkillBonuses :: [(CardCode, Int)]
encounterPhaseSkillBonuses = [("adventurous-spirit", 1)]

investigators :: [InvestigatorDef]
investigators =
  [ InvestigatorDef
      { id = "carson-sinclair"
      , name = "Carson Sinclair"
      , occupation = "The Butler"
      , expansion = UnderDarkWaves
      , health = 6
      , sanity = 6
      , focusLimit = Just 3
      , skills = skills 2 3 3 2 3
      , starting =
          [ StartingCard "anticipation"
          , StartingMoney 2
          , StartingChoice [[StartingCard "as-you-wish"], [StartingCard "prepared-for-anything"]]
          ]
      , roles = [Seeker, Survivor]
      , abilityText =
          "Quietly Indispensable—Once per round, while an investigator in any space is performing a test, you may spend one focus token to allow them to reroll one die. If you do, you recover one sanity."
      }
  , InvestigatorDef
      { id = "charlie-kane"
      , name = "Charlie Kane"
      , occupation = "The Politician"
      , expansion = UnderDarkWaves
      , health = 4
      , sanity = 8
      , -- his limit is however many allies he has, which 'focusLimitFromAllies' counts
        focusLimit = Nothing
      , skills = skills 2 4 3 2 2
      , starting =
          [ StartingCard "voice-of-authority"
          , StartingMoney 2
          , StartingChoice [[StartingCard "bonnie-walsh"], [StartingCard "calling-in-favors"]]
          ]
      , roles = [Rogue, Seeker]
      , abilityText =
          "Task Force—Action: Spend $2, and an additional $1 for each ally you have, to gain one ally. Leadership—Each time one of your allies is discarded, suffer one direct horror. Your focus limit is equal to the number of allies you have."
      }
  , InvestigatorDef
      { id = "father-mateo"
      , name = "Father Mateo"
      , occupation = "The Priest"
      , expansion = UnderDarkWaves
      , health = 5
      , sanity = 7
      , focusLimit = Just 3
      , skills = skills 3 3 1 2 4
      , starting =
          [ StartingCard "signum-crucis"
          , StartingMoney 2
          , StartingRemnants 1
          , StartingChoice [[StartingCard "hold-back-the-darkness"], [StartingCard "holy-water"]]
          ]
      , roles = [Mystic, Guardian]
      , abilityText =
          "Memento Mori—Once per round, while resolving a test, you may spend one remnant to reroll one or all of your dice. Blood of Martyrs—After you suffer one or more damage, you gain one remnant."
      }
  , InvestigatorDef
      { id = "patrice-hathaway"
      , name = "Patrice Hathaway"
      , occupation = "The Violinist"
      , expansion = UnderDarkWaves
      , health = 5
      , sanity = 7
      , focusLimit = Just 3
      , skills = skills 3 2 3 1 4
      , starting =
          [ StartingCard "patrices-violin"
          , StartingCard "captivating-melody"
          , StartingMoney 3
          , -- optional: the dreams come with the thing that sends them
            StartingChoice
              [
                [ StartingCard "ominous-dreams"
                , StartingEffect "shuffle The Watcher into the monster deck" (Custom "shuffle-in-the-watcher")
                ]
              , []
              ]
          ]
      , roles = [Mystic, Seeker]
      , abilityText =
          "Through the Eyes of the Watcher—Each time doom is placed on or moved to the scenario sheet, you may focus one skill of your choice, even if it exceeds your focus limit."
      }
  , InvestigatorDef
      { id = "silas-marsh"
      , name = "Silas Marsh"
      , occupation = "The Sailor"
      , expansion = UnderDarkWaves
      , health = 8
      , sanity = 4
      , focusLimit = Just 2
      , skills = skills 1 3 3 3 3
      , starting =
          [ StartingCard "fishing-net"
          , StartingMoney 3
          , StartingChoice [[StartingCard "adventurous-spirit"], [StartingCard "flannel-shirt"]]
          ]
      , roles = [Survivor, Rogue]
      , abilityText =
          "Tainted Blood—Once per round, when a ready monster activates, you may choose any investigator to be its prey. If that monster engages you, you may recover two sanity."
      }
  , InvestigatorDef
      { id = "stella-clark"
      , name = "Stella Clark"
      , occupation = "The Letter Carrier"
      , expansion = UnderDarkWaves
      , health = 5
      , sanity = 7
      , focusLimit = Just 2
      , skills = skills 3 2 3 2 3
      , starting =
          [ StartingCard "delivery-truck"
          , StartingMoney 3
          , StartingChoice [[StartingCard "snow-nor-rain"], [StartingCard "called-by-the-mists"]]
          ]
      , roles = [Survivor]
      , abilityText =
          "On the Job—At the beginning of your first action phase of the game, you may move directly to any space. Delivery Route—After you move three or more spaces during a single round, focus one skill of your choice."
      }
  , InvestigatorDef
      { id = "zoey-samaras"
      , name = "Zoey Samaras"
      , occupation = "The Chef"
      , expansion = UnderDarkWaves
      , health = 5
      , sanity = 7
      , focusLimit = Just 2
      , skills = skills 3 2 1 3 4
      , starting =
          [ StartingCard "chefs-knife"
          , StartingMoney 3
          , StartingChoice [[StartingCard "zoeys-cross"], [StartingCard "enchant-weapon"]]
          ]
      , roles = [Guardian, Mystic]
      , abilityText =
          "Grace of St. George—After you defeat a monster, you may remove one doom from your space or recover one health. Answer the Call—After you defeat a monster with elite, become BLESSED."
      }
  ]

starting
  :: CardCode -> Text -> AssetType -> [Trait] -> Int -> Maybe Int -> Maybe Int -> Text -> CardDef
starting c n ty traits hands health sanity txt =
  CardDef
    c
    n
    UnderDarkWaves
    1
    (AssetCard (AssetDef ty StartingPile traits Nothing hands health sanity castHorror txt))
 where
  -- 483.4: every starting spell costs one horror to cast
  castHorror = if ty == Spell then 1 else 0

cards :: [CardDef]
cards =
  [ starting
      "anticipation"
      "Anticipation"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "After you perform a focus action, you may focus one additional skill for each clue in your neighborhood."
  , starting
      "as-you-wish"
      "As You Wish"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "Once per round, after another investigator in any space performs an action, you may spend one focus to perform that same action. (Normal action restrictions apply.)"
  , starting
      "prepared-for-anything"
      "Prepared for Anything"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "Once per round, when an investigator in any space suffers any amount of damage or horror, you may discard one focus token to prevent that damage or horror."
  , starting
      "voice-of-authority"
      "Voice of Authority"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "Once per round, when resolving a test using a skill you have focused, you may test influence in place of the indicated skill. (Original modifiers still apply.)"
  , starting
      "bonnie-walsh"
      "Bonnie Walsh"
      Ally
      ["Faithful Assistant"]
      0
      (Just 2)
      (Just 3)
      "Once per round, before you resolve a test, you may focus one skill of your choice."
  , starting
      "calling-in-favors"
      "Calling in Favors"
      Talent
      ["Retainer"]
      0
      Nothing
      Nothing
      "At the start of your turn, discard all money from this card and test influence. For each success you roll, place $1 on this card. You may spend money from this card. (You may not trade it.)"
  , starting
      "signum-crucis"
      "Signum Crucis"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "Encounter: Test will -1. For each success you roll, choose an investigator in any space to discard one condition, recover one health, or recover one sanity."
  , starting
      "hold-back-the-darkness"
      "Hold Back the Darkness"
      Spell
      ["Incantation"]
      1
      Nothing
      Nothing
      "Once per round, before a reckoning effect resolves, you may test lore -1. If you succeed, do not resolve that reckoning effect this mythos phase."
  , starting
      "holy-water"
      "Holy Water"
      Item
      ["Curio"]
      1
      Nothing
      Nothing
      "You may treat the attack and evade modifiers of non-human monsters in your space as +1. After you damage a non-human monster during an attack action, exhaust that monster."
  , starting
      "patrices-violin"
      "Patrice's Violin"
      Item
      ["Curio"]
      2
      Nothing
      Nothing
      "After you perform a gather resources action, choose a skill. Each investigator in your neighborhood may focus that skill."
  , starting
      "captivating-melody"
      "Captivating Melody"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "You may perform a ward action while engaged with a monster. As part of a ward action, for each success you roll, you may exhaust one monster in your space instead of removing one doom."
  , starting
      "ominous-dreams"
      "Ominous Dreams"
      Talent
      ["Innate"]
      0
      (Just 2)
      (Just 2)
      "Once per round, while resolving a test, you may reroll one success to spawn or research one clue. If this talent is discarded, return The Watcher to the game box."
  , starting
      "fishing-net"
      "Fishing Net"
      Item
      ["Common", "Attachment"]
      1
      Nothing
      Nothing
      "During your turn, you may attach this item to a non-epic monster in your space to exhaust that monster. Attached: This monster cannot ready. After you defeat this monster, gain the attached item."
  , starting
      "adventurous-spirit"
      "Adventurous Spirit"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "During the encounter phase, each of your skills is increased by one."
  , starting
      "flannel-shirt"
      "Flannel Shirt"
      Item
      ["Common", "Curio"]
      0
      (Just 2)
      (Just 2)
      "Once per turn, when an investigator in your space resolves a test, you may deal one damage to this item to add two successes to their test result."
  , starting
      "delivery-truck"
      "Delivery Truck"
      Item
      ["Common", "Vehicle"]
      0
      Nothing
      Nothing
      "Each time you move out of a space, any unengaged investigators in that space may move with you. Action: Move up to two spaces. You may perform a trade action before or after this move as an additional action."
  , starting
      "snow-nor-rain"
      "Snow Nor Rain"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "Once per turn, when you would fail a test, you may discard one will focus to add one success to your test result."
  , starting
      "called-by-the-mists"
      "Called by the Mists"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "When you would draw a mythos token, you may instead suffer one direct horror and place one doom in your space."
  , starting
      "chefs-knife"
      "Chef's Knife"
      Item
      ["Common", "Weapon"]
      1
      Nothing
      Nothing
      "You get +2 strength as part of an attack action. After you reroll a die while resolving a test, add one to the result of that die."
  , starting
      "zoeys-cross"
      "Zoey's Cross"
      Item
      ["Curio"]
      1
      (Just 3)
      (Just 0)
      "After you become engaged with a monster, you may deal one damage to this item to test will. Deal damage to that monster equal to your test result."
  , starting
      "enchant-weapon"
      "Enchant Weapon"
      Spell
      ["Ritual", "Attachment"]
      0
      Nothing
      Nothing
      "At the start of your turn, you may test lore. If you pass, attach this card to a weapon in your space. Attached: Once per round, while performing an attack action, you may reroll one die or all dice."
  , -- Ashcan Pete's, printed in this box rather than the core set
    starting
      "wanderer"
      "Wanderer"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "Encounter: Once per round, you may choose an adjacent space. Resolve an encounter as though you are in that space, rolling one fewer die on any tests during that encounter."
  , theWatcher
  , theWatcherCondition
  ]

{- | Patrice's own monster. It carries no stats at all: the moment it would engage
anyone the card turns over and becomes the condition below, so it is never
fought. The speed star means "move directly to", which the engine walks as an
unlimited number of steps.
-}
theWatcher :: CardDef
theWatcher =
  CardDef
    "the-watcher"
    "The Watcher"
    UnderDarkWaves
    1
    ( MonsterCard
        MonsterDef
          { readyName = Nothing
          , spawn = PreySpace (NamedInvestigator "patrice-hathaway")
          , activation = Hunter (NamedInvestigator "patrice-hathaway")
          , speed = 99
          , traits = ["Unholy Presence"]
          , health = 1
          , elite = 0
          , attackSkill = Will
          , attackModifier = 0
          , evadeModifier = 0
          , damage = 0
          , horror = 0
          , remnant = False
          , keywords = [Relentless]
          , epic = False
          , text = "Spawn at Patrice Hathaway. Relentless. Hunter—Move directly to and engage Patrice Hathaway."
          }
    )

{- | The other side of that same card. It is filed as its own condition because the
engine keeps one kind per card code, and its back is the monster face.
-}
theWatcherCondition :: CardDef
theWatcherCondition =
  CardDef
    "the-watcher-condition"
    "The Watcher"
    UnderDarkWaves
    1
    ( ConditionCard
        ConditionDef
          { front =
              ConditionFace
                "THE WATCHER"
                "When The Watcher would engage you, gain this condition instead. While you are resolving a test, you must reroll one success. If you pass that test, you may spend one clue to discard this card."
          , back =
              ConditionFace
                "THE WATCHER"
                "Spawn at Patrice Hathaway. Relentless. Hunter—Move directly to and engage Patrice Hathaway."
          , backIsCondition = False
          , hiddenBack = False
          }
    )
