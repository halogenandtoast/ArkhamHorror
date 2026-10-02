-- | The four Secrets of the Order investigator sheets and the cards they start with.
module AH3e.Content.SecretsOfTheOrder.Investigators (investigators, cards) where

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
      { id = "agatha-crane"
      , name = "Agatha Crane"
      , occupation = "The Parapsychologist"
      , expansion = SecretsOfTheOrder
      , health = 5
      , sanity = 7
      , focusLimit = Just 2
      , skills = skills 4 3 3 1 2
      , starting =
          [ StartingCard "occult-principle"
          , StartingMoney 2
          , StartingChoice [[StartingCard "call-the-dead"], [StartingCard "spirit-camera"]]
          ]
      , roles = [Seeker, Mystic]
      , abilityText =
          "A New Field of Study—After you suffer one or more horror, you may focus one skill of your choice and gain one remnant."
      }
  , InvestigatorDef
      { id = "mark-harrigan"
      , name = "Mark Harrigan"
      , occupation = "The Soldier"
      , expansion = SecretsOfTheOrder
      , health = 8
      , sanity = 4
      , focusLimit = Just 1
      , skills = skills 1 2 2 4 4
      , starting =
          [ StartingCard "one-man-army"
          , StartingMoney 2
          , StartingChoice [[StartingCard "sophies-portrait"], [StartingCard "war-of-attrition"]]
          ]
      , roles = [Guardian]
      , abilityText =
          "Dogged—At the start of your turn, if you are delayed, you may focus one skill of your choice or recover one sanity. Steadfast—You may perform the attack action any number of times per round."
      }
  , InvestigatorDef
      { id = "preston-fairmont"
      , name = "Preston Fairmont"
      , occupation = "The Millionaire"
      , expansion = SecretsOfTheOrder
      , health = 7
      , sanity = 5
      , focusLimit = Just 3
      , skills = skills 2 5 2 3 1
      , starting =
          [ StartingCard "family-inheritance"
          , StartingMoney 4
          , StartingChoice [[StartingCard "money-talks"], [StartingCard "life-of-privilege"]]
          ]
      , roles = [Rogue, Survivor]
      , abilityText =
          "Creature Comforts—At the start of your turn, you may spend $1 to recover one health or one sanity."
      }
  , InvestigatorDef
      { id = "winifred-habbamock"
      , name = "Winifred Habbamock"
      , occupation = "The Aviatrix"
      , expansion = SecretsOfTheOrder
      , health = 6
      , sanity = 6
      , focusLimit = Just 3
      , skills = skills 1 2 4 2 4
      , starting =
          [ StartingMoney 3
          , -- "choose two", which is every pair of the three she is offered
            StartingChoice
              [ [StartingCard "anything-you-can-do", StartingCard "barnstormer"]
              , [StartingCard "anything-you-can-do", StartingCard "reckless-resolve"]
              , [StartingCard "barnstormer", StartingCard "reckless-resolve"]
              ]
          ]
      , roles = [Rogue]
      , abilityText =
          "Just That Good—Once per round, when you would fail a test, you may spend one focus to roll one additional die. If you still fail that test, choose a skill that you do not have focused; focus that skill twice."
      }
  ]

starting
  :: CardCode -> Text -> AssetType -> [Trait] -> Int -> Maybe Int -> Maybe Int -> Int -> Text -> CardDef
starting c n ty traits hands health sanity castHorror txt =
  CardDef
    c
    n
    SecretsOfTheOrder
    1
    (AssetCard (AssetDef ty StartingPile traits Nothing hands health sanity castHorror txt))

-- | A starting card with nothing to soak and nothing to pay for, which most are.
plain :: CardCode -> Text -> AssetType -> [Trait] -> Text -> CardDef
plain c n ty traits = starting c n ty traits 0 Nothing Nothing 0

cards :: [CardDef]
cards =
  [ {- Occult Principle is double sided, and its back is Scientific Method; the card
    is one card, so both sides' text is kept together here and the behaviour reads
    whichever side is showing. -}
    plain
      "occult-principle"
      "Occult Principle"
      Talent
      ["Innate"]
      "After you gain a clue from your neighborhood, you may place one doom in your space to gain one additional clue from the token pool. If you do, flip this talent.\nScientific Method: After you perform a ward action, if your test result was two or more, you may perform a research action as an additional action. If you do, flip this talent."
  , starting
      "call-the-dead"
      "Call the Dead"
      Spell
      ["Ritual"]
      0
      Nothing
      Nothing
      2
      "Action: Test lore -1. If you pass, discard the top card of your location's encounter deck. If you discard an event this way, gain one clue from your neighborhood."
  , plain
      "spirit-camera"
      "Spirit Camera"
      Item
      ["Curio"]
      "Once per round, when you would draw and resolve a mythos token, you may spend two remnants to spawn a clue instead. Place that clue's event card on top of its encounter deck (instead of shuffling it together with the top two cards)."
  , plain
      "one-man-army"
      "One Man Army"
      Talent
      ["Innate"]
      "After you end your movement in a monster's space, or after a monster moves into or spawns in your space, you may become delayed to perform an attack action as an additional action."
  , starting
      "sophies-portrait"
      "Sophie's Portrait"
      Item
      ["Curio"]
      0
      (Just 0)
      (Just 2)
      0
      "Once per round, while resolving a test, you may suffer one damage to reroll one die or all dice.\nWhen you use your Dogged ability, this item recovers one sanity."
  , plain
      "war-of-attrition"
      "War of Attrition"
      Talent
      ["Innate"]
      "At the end of your turn, you may suffer any amount of direct damage to deal damage equal to one less than that amount to each monster engaged with you. (You cannot voluntarily suffer damage in excess of your health.)"
  , plain
      "family-inheritance"
      "Family Inheritance"
      Talent
      ["Retainer"]
      "Reckoning—Gain $1 and all of the money on this card.\nAfter you perform a gather resources action, place an additional $2 on this card. (You cannot spend, use, or trade money on this card.)"
  , starting
      "money-talks"
      "Money Talks"
      Talent
      ["Innate"]
      0
      (Just 0)
      (Just 3)
      0
      "While you are resolving a test, you may spend $1 to reroll one die. (You can use this talent any number of times per test.)"
  , plain
      "life-of-privilege"
      "Life of Privilege"
      Talent
      ["Innate"]
      "After you focus a skill as part of a focus action, you may spend $1 to focus that skill one additional time."
  , plain
      "barnstormer"
      "Barnstormer"
      Talent
      ["Innate"]
      "Once per round, when you would pass a test, you may reroll all dice. If you do, become DRIVEN. If you are already DRIVEN, you may recover one health or one sanity instead."
  , plain
      "anything-you-can-do"
      "Anything You Can Do"
      Talent
      ["Innate"]
      "After an investigator in any space performs an action, you may become delayed to perform that same action. If that action requires a test, roll one more die than they did, instead of your normal dice pool. (Normal action restrictions still apply.)"
  , starting
      "reckless-resolve"
      "Reckless Resolve"
      Talent
      ["Innate"]
      0
      (Just 2)
      (Just 2)
      0
      "After you roll dice, you may become delayed to roll one additional die for each die that is not a success."
  ]
