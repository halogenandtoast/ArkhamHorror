module AH3e.Content.Items (cards) where

import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Ids

item :: CardCode -> Text -> Int -> [Trait] -> Int -> Text -> CardDef
item c n value traits hands = itemWith c n value traits hands Nothing Nothing

-- an item with a health/sanity bar of its own
itemWith :: CardCode -> Text -> Int -> [Trait] -> Int -> Maybe Int -> Maybe Int -> Text -> CardDef
itemWith c n value traits hands health sanity txt =
  CardDef
    c
    n
    CoreSet
    1
    (AssetCard (AssetDef Item ItemDeck traits (Just value) hands health sanity 0 txt))

cards :: [CardDef]
cards =
  [ item
      "shotgun"
      "Shotgun"
      6
      ["Common", "Weapon"]
      2
      "You get +5 strength as part of an attack action. Each 6 you roll as part of an attack action counts as two successes."
  , item
      "silver-key"
      "Silver Key"
      6
      ["Magical", "Curio"]
      0
      "Once per round, you may reroll any number of dice while resolving a lore or observation test."
  , item
      "tattered-cloak"
      "Tattered Cloak"
      5
      ["Magical", "Curio"]
      0
      "Non-epic monsters ignore you while activating and do not engage you unless you attack or damage them."
  , itemWith
      "token-of-faith"
      "Token of Faith"
      3
      ["Common", "Curio"]
      0
      (Just 0)
      (Just 3)
      "After this item suffers one or more horror, you recover one sanity."
  , itemWith
      "pain-killers"
      "Pain Killers"
      1
      ["Common"]
      0
      (Just 2)
      (Just 0)
      "Ignore the pain. The pain isn't what's going to kill ya."
  , item
      "pocket-watch"
      "Pocket Watch"
      6
      ["Common", "Curio"]
      0
      "You may perform one additional action during your turn."
  , item
      "rabbits-foot"
      "Rabbit's Foot"
      2
      ["Common", "Curio"]
      0
      "Once per round, you may reroll one die while resolving a test."
  , item "secret-page" "Secret Page" 3 ["Curio", "Tome"] 1 "You get +2 lore as part of a ward action."
  , item "mystic-scroll" "Mystic Scroll" 3 ["Curio", "Tome"] 0 "You get +2 lore while casting a spell."
  , item "mystic-tome" "Mystic Tome" 4 ["Curio", "Tome"] 1 "You get +3 lore while casting a spell."
  , item
      "occult-scripture"
      "Occult Scripture"
      3
      ["Curio", "Tome"]
      1
      "You get +2 observation as part of a research action."
  , item
      "otherworld-codex"
      "Otherworld Codex"
      5
      ["Curio", "Tome"]
      2
      "You get +3 lore as part of a ward action."
  , itemWith
      "leather-coat"
      "Leather Coat"
      3
      ["Common", "Curio"]
      0
      (Just 3)
      (Just 0)
      "You get +1 observation as part of an evade action."
  , item
      "lucky-cigarette-case"
      "Lucky Cigarette Case"
      5
      ["Common", "Curio"]
      0
      "Once per round, you may add one to the result of one die while resolving a test."
  , itemWith
      "liquid-courage"
      "Liquid Courage"
      1
      ["Common"]
      0
      (Just 0)
      (Just 2)
      "Too much and you lose your grip on reality. Too little and you only wish you had.."
  , item
      "magnifying-glass"
      "Magnifying Glass"
      1
      ["Common", "Curio"]
      1
      "You get +1 observation as part of a research action."
  , item
      "first-aid-kit"
      "First Aid Kit"
      2
      ["Common"]
      0
      "Action: An investigator or ally in your space recovers one health. (You are an investigator in your space.)"
  , item
      "grimms-fairy-tales"
      "Grimms' Fairy Tales"
      4
      ["Common", "Curio", "Tome"]
      0
      "Action: An investigator or ally in your space recovers one sanity. (You are an investigator in your space.)"
  , item
      "grotesque-statue"
      "Grotesque Statue"
      4
      ["Magical", "Curio"]
      0
      "After you perform a research action, you may suffer one horror to research one clue. (Place it on the scenario sheet.)"
  , item
      "knife"
      "Knife"
      1
      ["Common", "Curio", "Weapon"]
      1
      "You get +1 strength as part of an attack action."
  , itemWith
      "bulletproof-vest"
      "Bulletproof Vest"
      4
      ["Common"]
      0
      (Just 4)
      (Just 0)
      "Once per round, if two or more damage would be dealt to this card, prevent one of that damage."
  , itemWith
      "elder-sign-amulet"
      "Elder Sign Amulet"
      4
      ["Magical", "Curio"]
      0
      (Just 2)
      (Just 4)
      "Once per round, if two or more horror would be dealt to this card, prevent one of that horror."
  , item
      "dynamite"
      "Dynamite"
      5
      ["Common", "Weapon"]
      0
      "As part of an attack action, you may discard this card to deal five damage to each monster you are engaged with."
  , item
      "fine-clothes"
      "Fine Clothes"
      4
      ["Common", "Curio"]
      0
      "Once per round, you may pay half price (rounded up) while buying an item. (Does not stack with other cost reduction.)"
  , item
      "38-revolver"
      ".38 Revolver"
      3
      ["Common", "Weapon"]
      1
      "You get +2 strength as part of an attack action."
  , item
      "45-automatic"
      "45 Automatic"
      4
      ["Common", "Weapon"]
      1
      "You get +3 strength as part of an attack action."
  , item
      "41-derringer"
      "41 Derringer"
      2
      ["Common", "Curio", "Weapon"]
      1
      "You may add one to the result of one die as part of an attack action."
  , item
      "45-thompson"
      "45 Thompson"
      5
      ["Common", "Weapon"]
      2
      "You get +5 strength as part of an attack action."
  ]
