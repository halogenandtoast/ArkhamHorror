module AH3e.Content.Core.Investigators (investigators, cards) where

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
      { id = "ashcan-pete"
      , name = "\"Ashcan\" Pete"
      , occupation = "The Drifter"
      , expansion = CoreSet
      , health = 7
      , sanity = 5
      , focusLimit = Just 3
      , skills = skills 3 1 3 3 3
      , starting =
          [ StartingCard "duke"
          , StartingMoney 1
          , StartingChoice [[StartingCard "petes-guitar"], [StartingCard "dark-dreams"]]
          ]
      , roles = [Survivor]
      , abilityText =
          "Scrounge—After you perform a gather resources action, you may gain an item worth $2 or less from the display."
      }
  , InvestigatorDef
      { id = "daniela-reyes"
      , name = "Daniela Reyes"
      , occupation = "The Mechanic"
      , expansion = CoreSet
      , health = 7
      , sanity = 5
      , focusLimit = Just 3
      , skills = skills 3 3 1 3 3
      , starting =
          [ StartingCard "gabriel"
          , StartingMoney 3
          , StartingChoice [[StartingCard "ace-of-swords"], [StartingCard "wrench"]]
          ]
      , roles = []
      , abilityText =
          "Love For the Job—After you perform a gather resources action, you may focus one skill of your choice."
      }
  , InvestigatorDef
      { id = "agnes-baker"
      , name = "Agnes Baker"
      , occupation = "The Waitress"
      , expansion = CoreSet
      , health = 6
      , sanity = 6
      , focusLimit = Just 2
      , skills = skills 4 2 2 2 3
      , starting =
          [ StartingCard "heirloom-of-hyperborea"
          , StartingMoney 3
          , StartingChoice [[StartingCard "storm-of-spirits"], [StartingCard "flesh-ward"]]
          ]
      , roles = [Mystic, Guardian]
      , abilityText =
          "Blood Casting\8212You may suffer damage instead of horror while casting a spell. Blood is Power\8212While casting a spell, if you suffer damage or spend remnants, you get +2 will."
      }
  , InvestigatorDef
      { id = "calvin-wright"
      , name = "Calvin Wright"
      , occupation = "The Haunted"
      , expansion = CoreSet
      , health = 6
      , sanity = 6
      , focusLimit = Just 2
      , skills = skills 3 3 3 3 1
      , starting =
          [ StartingCard "spirit-dagger"
          , StartingCard "until-the-end-of-time"
          , StartingMoney 3
          , -- optional: he may take the messenger's voice and the pact that comes with it
            StartingChoice [[StartingCard "voice-of-the-messenger", StartingCondition "DARK PACT"], []]
          ]
      , roles = [Guardian, Rogue]
      , abilityText =
          "Friend Indeed\8212Action: You may exchange any amount of health and/or sanity with another investigator or ally in any space."
      }
  , InvestigatorDef
      { id = "dexter-drake"
      , name = "Dexter Drake"
      , occupation = "The Magician"
      , expansion = CoreSet
      , health = 5
      , sanity = 7
      , focusLimit = Nothing
      , skills = skills 4 3 2 2 2
      , starting =
          [ StartingCard "mists-of-rlyeh"
          , StartingMoney 2
          , StartingChoice [[StartingCard "astral-travel"], [StartingCard "magicians-cane"]]
          ]
      , roles = [Mystic]
      , abilityText =
          "Magical Gift\8212Once per round, while resolving a will test, you may reroll one or all of your dice. Your focus limit is equal to the number of spells you have."
      }
  , InvestigatorDef
      { id = "jenny-barnes"
      , name = "Jenny Barnes"
      , occupation = "The Dilettante"
      , expansion = CoreSet
      , health = 7
      , sanity = 5
      , focusLimit = Just 1
      , skills = skills 1 4 2 3 3
      , starting =
          [ StartingCard "search-for-izzie"
          , StartingMoney 5
          , StartingChoice [[StartingCard "dressed-to-the-nines"], [StartingCard "jennys-twin-45s"]]
          ]
      , roles = [Rogue]
      , abilityText = "Trust Fund\8212Action: If you have fewer than $3, you gain $3."
      }
  , InvestigatorDef
      { id = "marie-lambeau"
      , name = "Marie Lambeau"
      , occupation = "The Entertainer"
      , expansion = CoreSet
      , health = 5
      , sanity = 7
      , focusLimit = Just 2
      , skills = skills 3 4 2 2 2
      , starting =
          [ StartingCard "intervene"
          , StartingMoney 4
          , StartingChoice [[StartingCard "witch-blood"], [StartingCard "grande-meres-knife"]]
          ]
      , roles = [Mystic, Survivor]
      , abilityText =
          "Smoky Velvet\8212Once per round, after you perform an action, another investigator on any space may perform that same action. (Normal action restrictions still apply.)"
      }
  , InvestigatorDef
      { id = "michael-mcglen"
      , name = "Michael McGlen"
      , occupation = "The Mobster"
      , expansion = CoreSet
      , health = 8
      , sanity = 4
      , focusLimit = Just 1
      , skills = skills 2 3 1 4 3
      , starting =
          [ StartingCard "chicago-typewriter"
          , StartingMoney 3
          , StartingChoice [[StartingCard "obannion-member"], [StartingCard "ol-boiler"]]
          ]
      , roles = [Rogue, Guardian]
      , abilityText =
          "Out for Revenge\8212After you defeat a monster as part of an attack action, you recover one sanity or focus one skill of your choice."
      }
  , InvestigatorDef
      { id = "minh-thi-phan"
      , name = "Minh Thi Phan"
      , occupation = "The Secretary"
      , expansion = CoreSet
      , health = 6
      , sanity = 6
      , focusLimit = Just 2
      , skills = skills 3 3 3 2 2
      , starting =
          [ StartingCard "king-in-yellow"
          , StartingMoney 3
          , StartingChoice [[StartingCard "analytical-mind"], [StartingCard "synergy"]]
          ]
      , roles = [Seeker, Survivor]
      , abilityText =
          "All Around You\8212Once per round, while resolving a test, you or another investigator in your space may reroll dice up to the number of clues in your neighborhood."
      }
  , InvestigatorDef
      { id = "norman-withers"
      , name = "Norman Withers"
      , occupation = "The Astronomer"
      , expansion = CoreSet
      , health = 5
      , sanity = 7
      , focusLimit = Just 2
      , skills = skills 3 1 3 2 4
      , starting =
          [ StartingCard "find-gate"
          , StartingMoney 1
          , StartingChoice [[StartingCard "astronomy-book"], [StartingCard "precious-memento"]]
          ]
      , roles = [Seeker, Mystic]
      , abilityText =
          "In the Stars\8212After you remove two or more doom from your space, you may suffer one horror to research one clue (place it on the scenario sheet)."
      }
  , InvestigatorDef
      { id = "rex-murphy"
      , name = "Rex Murphy"
      , occupation = "The Reporter"
      , expansion = CoreSet
      , health = 7
      , sanity = 7
      , focusLimit = Nothing
      , skills = skills 3 3 3 2 2
      , starting =
          [ StartingCard "search-for-the-truth"
          , StartingCard "the-tower"
          , StartingMoney 3
          , StartingChoice [[StartingCard "it-all-comes-together"], [StartingCard "overcome-all-odds"]]
          ]
      , roles = [Seeker, Rogue]
      , abilityText =
          "Family Curse\8212While resolving a test, only 6s count as successes. You cannot become BLESSED or CURSED. Never Give Up\8212After you fail a test, you focus one skill of your choice."
      }
  , InvestigatorDef
      { id = "tommy-muldoon"
      , name = "Tommy Muldoon"
      , occupation = "The Rookie Cop"
      , expansion = CoreSet
      , health = 7
      , sanity = 5
      , focusLimit = Just 2
      , skills = skills 2 2 3 3 3
      , starting =
          [ StartingCard "becky"
          , StartingMoney 2
          , StartingChoice [[StartingCard "handcuffs"], [StartingCard "motorcycle"]]
          ]
      , roles = [Guardian, Survivor]
      , abilityText =
          "Shield From Harm\8212If a monster would engage another investigator in your space, you may engage that monster instead."
      }
  , InvestigatorDef
      { id = "wendy-adams"
      , name = "Wendy Adams"
      , occupation = "The Urchin"
      , expansion = CoreSet
      , health = 5
      , sanity = 7
      , focusLimit = Just 3
      , skills = skills 3 1 4 2 3
      , starting =
          [ StartingCard "mamas-amulet"
          , StartingMoney 1
          , StartingChoice [[StartingCard "mr-pawterson"], [StartingCard "mysterious-photo"]]
          ]
      , roles = [Survivor, Seeker]
      , abilityText =
          "Shortcut\8212Before or after you perform an additional action as part of an evade action, you may move up to two spaces (for free)."
      }
  ]

starting
  :: CardCode -> Text -> AssetType -> [Trait] -> Int -> Maybe Int -> Maybe Int -> Text -> CardDef
starting c n ty traits hands health sanity txt =
  CardDef
    c
    n
    CoreSet
    1
    (AssetCard (AssetDef ty StartingPile traits Nothing hands health sanity castHorror txt))
 where
  -- 483.4: every starting spell costs one horror to cast
  castHorror = if ty == Spell then 1 else 0

cards :: [CardDef]
cards =
  [ starting
      "duke"
      "Duke"
      Ally
      ["Faithful Hound"]
      0
      (Just 2)
      (Just 3)
      "You get +1 strength as part of an attack action. When you perform a trade action, you may trade assets with any investigators in your neighborhood."
  , starting
      "petes-guitar"
      "Pete's Guitar"
      Item
      ["Common", "Curio"]
      0
      Nothing
      Nothing
      "After you perform a gather resources action, a number of investigators in your neighborhood equal to your influence may each recover one sanity or focus one skill of their choice."
  , starting
      "dark-dreams"
      "Dark Dreams"
      Talent
      []
      0
      Nothing
      Nothing
      "When you draw a blank mythos token, you may suffer one direct horror to focus one skill of your choice and spawn one clue."
  , starting
      "gabriel"
      "Gabriel"
      Item
      ["Vehicle"]
      0
      Nothing
      Nothing
      "Instead of a normal move action, you move up to three spaces and may spend $1 to move one additional space."
  , starting
      "wrench"
      "Wrench"
      Item
      ["Common"]
      1
      Nothing
      Nothing
      "You get +1 strength as part of an attack action. You get +3 instead if you have a free hand."
  , starting
      "ace-of-swords"
      "Ace of Swords"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "When you spend a focus to reroll a die, you may recover one sanity."
  , starting
      "heirloom-of-hyperborea"
      "Heirloom of Hyperborea"
      Item
      ["Magical", "Curio"]
      0
      Nothing
      Nothing
      "After you cast a spell, you may focus one skill of your choice."
  , starting
      "storm-of-spirits"
      "Storm of Spirits"
      Spell
      ["Incantation"]
      0
      Nothing
      Nothing
      "You may test lore in place of strength as part of an attack action. (The monster's attack modifier still applies.)"
  , starting
      "spirit-dagger"
      "Spirit Dagger"
      Item
      ["Magical", "Curio"]
      1
      Nothing
      Nothing
      "You get +2 strength as part of an attack action. You get +2 will as part of a ward action."
  , starting
      "until-the-end-of-time"
      "Until the End of Time"
      ConditionAsset
      ["Innate"]
      0
      (Just 2)
      (Just 2)
      "This card cannot be discarded by any means. (Including damage and horror.) Reckoning—Remove one damage and one horror from this card."
  , starting
      "voice-of-the-messenger"
      "Voice of the Messenger"
      ConditionAsset
      ["Innate"]
      0
      Nothing
      Nothing
      "You may suffer one horror to reroll any number of dice. (Use this no more than once per roll. This allows you to reroll the die rolled for your dark pact.)"
  , starting
      "magicians-cane"
      "Magician's Cane"
      Item
      ["Curio"]
      1
      (Just 2)
      (Just 2)
      "You get +2 will while casting a spell."
  , starting
      "search-for-izzie"
      "Search for Izzie"
      Talent
      ["Innate"]
      0
      (Just 2)
      (Just 2)
      "Once per round, while resolving a test, you may suffer one damage and one horror to reroll any number of dice."
  , starting
      "dressed-to-the-nines"
      "Dressed to the Nines"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "When you spend a focus token to reroll a die, you may reroll any number of dice instead. You gain an additional $2 as part of your trust fund ability."
  , starting
      "jennys-twin-45s"
      "Jenny's Twin 45s"
      Item
      ["Common", "Weapon"]
      2
      Nothing
      Nothing
      "You get +3 strength as part of an attack action. You may add one to the result of one die as part of an attack action."
  , starting
      "witch-blood"
      "Witch Blood"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "Action: You may perform an action you have already performed this round. You can allow another investigator to perform this action with your Smoky Velvet ability. Once per round, after you spend a remnant, you gain one remnant."
  , starting
      "grande-meres-knife"
      "Grande-mère's Knife"
      Item
      ["Magical", "Weapon"]
      1
      Nothing
      Nothing
      "You get +2 strength as part of an attack action. You get +2 will while casting a spell."
  , starting
      "chicago-typewriter"
      "Chicago Typewriter"
      Item
      ["Common", "Weapon"]
      2
      Nothing
      Nothing
      "You get +4 strength as part of an attack action."
  , starting
      "obannion-member"
      "O'Bannion Member"
      Talent
      ["Retainer"]
      0
      Nothing
      Nothing
      "After you perform a gather resources action, you may test strength. If you pass, you gain $2."
  , starting
      "ol-boiler"
      "Ol' Boiler"
      Item
      ["Vehicle"]
      0
      (Just 5)
      (Just 0)
      "After you perform a move action, you may deal two damage to one monster you are engaged with and two damage to this card."
  , starting
      "king-in-yellow"
      "King in Yellow"
      Item
      ["Curio", "Tome"]
      0
      Nothing
      Nothing
      "Once per round, after you suffer one or more horror, you may research one clue (place it on the scenario sheet)."
  , starting
      "analytical-mind"
      "Analytical Mind"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "You roll one additional die while resolving a will or observation test if you are at or above your focus limit."
  , starting
      "synergy"
      "Synergy"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "Increase your focus limit by one. Other investigators in your space get +1 to each skill you have focused."
  , starting
      "astronomy-book"
      "Astronomy Book"
      Item
      ["Curio", "Tome"]
      1
      Nothing
      Nothing
      "Once per round, while resolving a test, you may reroll a number of dice up to the amount of doom in your space."
  , starting
      "precious-memento"
      "Precious Memento"
      Item
      ["Curio"]
      0
      (Just 0)
      (Just 3)
      "After you gain a clue, remove two horror from this card."
  , starting
      "search-for-the-truth"
      "Search for the Truth"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "After you gain a clue, you gain $1 and you may focus one skill of your choice."
  , starting
      "the-tower"
      "The Tower"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "Once per round, while resolving a test, you may reroll one or all of your dice."
  , starting
      "it-all-comes-together"
      "It All Comes Together"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "As part of a research action, add one to the result of each die you roll."
  , starting
      "overcome-all-odds"
      "Overcome All Odds"
      Talent
      ["Innate"]
      0
      Nothing
      Nothing
      "You can focus each skill up to twice."
  , starting
      "becky"
      "Becky"
      Item
      ["Weapon"]
      2
      (Just 2)
      (Just 3)
      "You get +4 strength as part of an attack action."
  , starting
      "handcuffs"
      "Handcuffs"
      Item
      ["Common"]
      0
      Nothing
      Nothing
      "Once per round, after you damage, disengage, or are damaged by a non-epic human monster, you may defeat that monster."
  , starting
      "motorcycle"
      "Motorcycle"
      Item
      ["Vehicle"]
      0
      Nothing
      Nothing
      "Instead of a normal move action, you move up to three spaces and may spend $1 to move one additional space."
  , starting
      "mamas-amulet"
      "Mama's Amulet"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "Once per round, you may prevent one damage or horror that you would suffer."
  , starting
      "mr-pawterson"
      "Mr. Pawterson"
      Item
      ["Curio"]
      0
      (Just 2)
      (Just 2)
      "Before one or more monsters would attack you, you may discard this card to disengage and exhaust those monsters."
  , starting
      "mysterious-photo"
      "Mysterious Photo"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "After you gain a clue, you may focus one skill of your choice."
  ]
