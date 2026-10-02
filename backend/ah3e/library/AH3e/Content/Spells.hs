module AH3e.Content.Spells (cards) where

import AH3e.Content.Vocabulary (fromBox)
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Ids

-- spells are cast with lore; `horror` is the horror dealt to the card on a cast
spell :: CardCode -> Text -> Int -> Trait -> Int -> Int -> Text -> CardDef
spell c n value trait hands horror txt =
  CardDef
    c
    n
    CoreSet
    1
    (AssetCard (AssetDef Spell SpellDeck [trait] (Just value) hands Nothing Nothing horror txt))

cards :: [CardDef]
cards = core <> fromBox DeadOfNight deadOfNight <> fromBox SecretsOfTheOrder secretsOfTheOrder

core :: [CardDef]
core =
  [ spell
      "mists-of-rlyeh"
      "Mists of R'lyeh"
      1
      "Incantation"
      0
      1
      "You may test lore in place of observation as part of an evade action. (The monster's evade modifier still applies.)"
  , spell
      "shriveling"
      "Shriveling"
      3
      "Ritual"
      0
      1
      "Action: Test lore -1. One monster in your space or an adjacent space suffers damage equal to your test result. You can perform this action while engaged with a monster."
  , spell
      "wither"
      "Wither"
      2
      "Incantation"
      1
      1
      "As part of an attack action, you may test lore -1. Add your test result to the test result of the attack action."
  , spell
      "flesh-ward"
      "Flesh Ward"
      3
      "Incantation"
      1
      1
      "Once per round, if you, another investigator, or an ally on any space would suffer damage, you may test lore. Prevent damage equal to your test result."
  , spell
      "intervene"
      "Intervene"
      4
      "Incantation"
      2
      2
      "Once per round, while another investigator on any space is resolving a test, you may test lore -1. Add your test result to the other investigator's test result."
  , spell
      "healing-words"
      "Healing Words"
      2
      "Ritual"
      2
      1
      "Action: Test lore -1. An investigator or ally in your space recovers health equal to your test result. (You are an investigator in your space.)"
  , spell
      "astral-travel"
      "Astral Travel"
      2
      "Incantation"
      1
      1
      "Instead of a normal move action, you may test lore. You move a number of spaces equal to your test result plus two."
  , spell
      "find-gate"
      "Find Gate"
      4
      "Ritual"
      0
      2
      "Action: Test lore. If you pass, you move to a space of your choice with one or more doom."
  , spell
      "alchemical-process"
      "Alchemical Process"
      1
      "Ritual"
      0
      1
      "Action: Test lore. You gain $1 for each success you roll."
  , spell
      "binding"
      "Binding"
      3
      "Ritual"
      0
      1
      "Action: Choose one monster on any space and test lore using that monster's evade modifier. If you pass, exhaust that monster. (It disengages.)"
  ]

-- Dead of Night
deadOfNight :: [CardDef]
deadOfNight =
  [ spell
      "clairvoyance"
      "Clairvoyance"
      2
      "Incantation"
      0
      1
      "At the start of your turn, you may test lore. Look at the top card of a number of neighborhood decks up to your test result. Of those, you may discard one non-event card."
  , spell
      "instill-bravery"
      "Instill Bravery"
      4
      "Incantation"
      1
      1
      "Once per round, if you, another investigator, or an ally on any space would suffer horror, you may test lore. Prevent horror equal to your test result."
  , spell
      "lure-monster"
      "Lure Monster"
      2
      "Incantation"
      0
      1
      "Once per round, when a ready, non-epic monster would activate, you may test lore. If you pass, move it two spaces toward you instead."
  , spell
      "wrack"
      "Wrack"
      2
      "Ritual"
      1
      1
      "Action: Test lore +1. Defeat one monster in your space with remaining health equal to or lower than your test result. You may perform this action while engaged with a monster."
  ]

-- Secrets of the Order
secretsOfTheOrder :: [CardDef]
secretsOfTheOrder =
  [ spell
      "banishment"
      "Banishment"
      2
      "Incantation"
      0
      1
      "Once per round, during your turn, you may choose a non-epic monster and test lore -1. If you pass, that monster disengages all investigators and moves directly to the unstable space."
  , spell
      "the-beast-within"
      "The Beast Within"
      3
      "Incantation"
      0
      1
      "When you perform an attack action, you may test lore. If you pass, roll five dice instead of your usual dice pool. Ignore all other modifiers."
  ]
