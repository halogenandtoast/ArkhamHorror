module AH3e.Content.Headlines (cards) where

import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill

-- the number each takes is the one printed in its bottom-right corner, which its art is filed under
headline :: CardCode -> Int -> Text -> Text -> Effect -> CardDef
headline code n name txt eff = CardDef code name CoreSet 1 (HeadlineCard (HeadlineDef n False txt eff Nothing))

rumor :: CardCode -> Int -> Text -> Text -> Maybe Effect -> CardDef
rumor code n name txt = rumorWith code n name txt NoEffect

-- | A rumor that also does something the once, as it goes into the codex.
rumorWith :: CardCode -> Int -> Text -> Text -> Effect -> Maybe Effect -> CardDef
rumorWith code n name txt eff reck =
  CardDef code name CoreSet 1 (HeadlineCard (HeadlineDef n True txt eff reck))

doomHere, spawnHere, spawnUnstable :: Effect
doomHere = PlaceDoomAt YourSpace (N 1)
spawnHere = SpawnMonsterIn YourSpace False
spawnUnstable = SpawnMonsterIn TheUnstableSpace False

graded :: Skill -> Effect -> Effect -> Effect -> Effect
graded sk zero oneTwo threePlus = Test sk 0 (ByResult [((1, Just 2), oneTwo), ((3, Nothing), threePlus)]) zero

unless' :: Text -> Effect -> Text -> Effect -> Effect
unless' sufferLabel suffer otherLabel other = Choose [(sufferLabel, suffer), (otherLabel, other)]

cards :: [CardDef]
cards = core <> fromBox DeadOfNight deadOfNight

core :: [CardDef]
core =
  [ rumor
      "astronomers-chuffed-as-stars-align"
      31
      "Astronomers Chuffed as Stars Align"
      "Add this card to the codex and discard all other rumor headlines. Reckoning—Each investigator tests (will). Each investigator who fails places one doom in their space. Then any investigator may spend one clue to discard this card."
      (Just (Custom "rumor-astronomers"))
  , headline
      "asylum-overflowing"
      13
      "Asylum Overflowing"
      "You suffer one horror for each doom in your space."
      (SufferHorror (Counted DoomInYourSpace))
  , headline
      "banned-books-bandied-about"
      15
      "Banned Books Bandied About"
      "For each clue you have, you suffer one horror unless you discard that clue."
      ( ForEachOf
          CluesYouHave
          (unless' "Suffer one horror" (horror 1) "Discard the clue" (Pay (SpendClues 1) NoEffect))
      )
  , headline
      "big-city-burglars-busted"
      7
      "Big City Burglars Busted"
      "For each item you have, you suffer one damage unless you discard that item."
      (Custom "big-city-burglars-busted")
  , headline
      "break-in-at-historical-society"
      11
      "Break-In at Historical Society"
      "You suffer two damage and two horror unless you place one doom in your space. (If you are in a street space, place the doom in an adjacent neighborhood space instead.)"
      ( unless'
          "Suffer two damage and two horror"
          (harm 2 2)
          "Place one doom in your space"
          doomHere
      )
  , headline
      "church-leaders-bless-city"
      6
      "Church Leaders Bless City"
      "Test (will) and resolve the effect based on your test result: 0: Spawn one monster in your space. 1–2: You become BLESSED. Spawn one monster in your space. 3+: You become BLESSED. (The monster engages you or another investigator in your space.)"
      (graded Will spawnHere (Seq [blessed, spawnHere]) blessed)
  , headline
      "concerned-citizens-congregate"
      5
      "Concerned Citizens Congregate"
      "Test (influence) and resolve the effect based on your test result: 0: You place one doom in your space. 1–2: You gain one ally and place one doom in your space. 3+: You gain one ally. (If you are in a street space, place the doom in an adjacent neighborhood space instead.)"
      (graded Influence doomHere (Seq [ally, doomHere]) ally)
  , headline
      "curfew-established"
      22
      "Curfew Established"
      "You suffer one damage for each monster in your neighborhood."
      (SufferDamage (Counted MonstersInYourNeighborhood))
  , headline
      "cursed-treasure-brings-money-problems"
      2
      "Cursed Treasure Brings Money Problems"
      "Test (will) and resolve the effect based on your test result: 0: You become CURSED. 1–2: You gain $3 and become CURSED. 3+: You gain $3."
      (graded Will cursed (Seq [money 3, cursed]) (money 3))
  , headline
      "dream-expert-found-dead"
      10
      "Dream Expert Found Dead"
      "Test (will). You suffer three horror; prevent one horror for each success you rolled."
      (let e = SufferHorror (Diff (N 3) TestResult) in Test Will 0 e e)
  , headline
      "foul-footprints-found"
      20
      "Foul Footprints Found"
      "Test (will) and resolve the effect based on your test result: 0: Spawn one monster at the unstable space. 1–2: You gain one remnant. Spawn one monster at the unstable space. 3+: You gain one remnant."
      (graded Will spawnUnstable (Seq [remnants 1, spawnUnstable]) (remnants 1))
  , rumor
      "full-moon-linked-to-aberrant-behavior"
      30
      "Full Moon Linked to Aberrant Behavior"
      "Add this card to the codex and discard all other rumor headlines. Reckoning—Each investigator tests (will). Each investigator who fails suffers two horror. Then any investigator may spend one clue to discard this card."
      (Just (Custom "rumor-full-moon"))
  , headline
      "its-curtains-for-arkham"
      26
      "It's Curtains for Arkham"
      "You suffer three horror unless you place two doom in your space. (If you are in a street space, place the doom in an adjacent neighborhood space instead.)"
      ( unless'
          "Suffer three horror"
          (horror 3)
          "Place two doom in your space"
          (PlaceDoomAt YourSpace (N 2))
      )
  , headline
      "magician-vanishes-rabbit-self"
      24
      "Magician Vanishes Rabbit, Self"
      "You disengage all monsters and move to the unstable space. (The monsters are not exhausted. They engage another investigator in their space.)"
      (Custom "magician-vanishes")
  , headline
      "masked-man-mystery"
      19
      "Masked Man Mystery"
      "Choose another investigator in any space. Spawn one monster in that investigator's space. (It engages that investigator or another investigator in that space.)"
      (Custom "masked-man-mystery")
  , headline
      "miskatonic-museum-mystery"
      4
      "Miskatonic Museum Mystery"
      "Test (will) and resolve the effect based on your test result: 0: You become CURSED. 1–2: You gain one curio item and become CURSED. 3+: You gain one curio item."
      (graded Will cursed (Seq [curioItem, cursed]) curioItem)
  , headline
      "mysticism-malfeasance"
      3
      "Mysticism Malfeasance"
      "Test (will −1). If you fail, place one doom in your space. (If you are in a street space, place the doom in an adjacent neighborhood space instead.)"
      (Test Will (-1) NoEffect doomHere)
  , headline
      "night-terrors-terrify-nightly"
      16
      "Night Terrors Terrify Nightly"
      "Test (will). You suffer three damage; prevent one damage for each success you rolled."
      (let e = SufferDamage (Diff (N 3) TestResult) in Test Will 0 e e)
  , headline
      "northside-vigilante-apprehended"
      25
      "Northside Vigilante Apprehended"
      "You choose one: • You suffer two damage and two horror. • You become CURSED."
      (Choose [("Suffer two damage and two horror", harm 2 2), ("Become CURSED", cursed)])
  , headline
      "occult-activity-threatens-city"
      18
      "Occult Activity Threatens City"
      "You choose one: • Place one doom in your space. (If you are in a street space, place the doom in an adjacent neighborhood space instead.) • Spawn one monster in your space. (It engages you or another investigator in your space.)"
      (Choose [("Place one doom in your space", doomHere), ("Spawn one monster in your space", spawnHere)])
  , headline
      "police-do-your-jobs"
      14
      "Police, Do Your Jobs!"
      "For each clue in your neighborhood you suffer one damage or one horror."
      ( ForEachOf
          CluesInYourNeighborhood
          (Choose [("Suffer one damage", damage 1), ("Suffer one horror", horror 1)])
      )
  , headline
      "purse-snatchers-pursued"
      17
      "Purse Snatchers Pursued"
      "You suffer two damage unless you discard one item."
      (unless' "Suffer two damage" (damage 2) "Discard one item" (Pay (CostDiscard ItemCard) NoEffect))
  , rumor
      "rogue-comet-approaches"
      32
      "Rogue Comet Approaches"
      "Add this card to the codex and discard all other rumor headlines. Reckoning—Any investigator may spend one clue to discard this card. Otherwise, place one doom on this card. When there is three doom on this card, resolve a gate burst mythos effect. Then, discard this card."
      (Just (Custom "rumor-comet"))
  , headline
      "so-called-pharaoh-speaks-in-cambridge"
      27
      "So-Called Pharaoh Speaks in Cambridge"
      "You suffer three damage and three horror unless you gain a DARK PACT condition."
      ( unless'
          "Suffer three damage and three horror"
          (harm 3 3)
          "Gain a DARK PACT"
          (Pay (CostCondition "DARK PACT") NoEffect)
      )
  , headline
      "starstruck-scientists-stumped"
      8
      "Starstruck Scientists Stumped"
      "For each spell you have, you suffer one horror unless you discard that spell."
      ( ForEachOf
          SpellsYouHave
          (unless' "Suffer one horror" (horror 1) "Discard the spell" (Pay (CostDiscard SpellCard) NoEffect))
      )
  , headline
      "symposium-snafu"
      23
      "Symposium Snafu"
      "If you have one or more clues, you become CURSED."
      (If (HasClues 1) cursed NoEffect)
  , rumor
      "truckers-strike-leads-to-shortages"
      29
      "Truckers' Strike Leads to Shortages"
      "Add this card to the codex and discard all other rumor headlines. The value of each item in the display is increased by two for as long as this card is in the codex. Before you would buy one or more items from the display, you may test (influence −1). If you pass, you ignore this card this round."
      Nothing
  , headline
      "unexplained-sightings-near-river"
      12
      "Unexplained Sightings Near River"
      "You suffer one damage for each doom in your space."
      (SufferDamage (Counted DoomInYourSpace))
  , headline
      "unscheduled-parade-shuts-down-main-st"
      21
      "Unscheduled Parade Shuts Down Main St."
      "You suffer one horror for each monster in your neighborhood."
      (SufferHorror (Counted MonstersInYourNeighborhood))
  , headline
      "we-have-no-one-to-blame-but-ourselves"
      1
      "We Have No One to Blame but Ourselves"
      "Place two doom in your space unless you gain a DARK PACT condition. (If you are in a street space, place the doom in an adjacent neighborhood space instead.)"
      ( unless'
          "Place two doom in your space"
          (PlaceDoomAt YourSpace (N 2))
          "Gain a DARK PACT"
          (Pay (CostCondition "DARK PACT") NoEffect)
      )
  , headline
      "why-even-go-on-anymore"
      28
      "Why Even Go On Anymore?"
      "You roll one die. (This die roll is not a test.) You suffer a total amount of damage and/or horror equal to the die result. (You choose how to split the die result between damage and horror.)"
      (Custom "why-even-go-on")
  , headline
      "wild-animal-attacks-spike"
      9
      "Wild Animal Attacks Spike"
      "Choose one non-epic monster in any space. You suffer damage equal to that monster's remaining health. Then you defeat it."
      (Custom "wild-animal-attacks")
  ]

-- Dead of Night
wanted :: Effect
wanted = GainE (Condition "WANTED")

deadOfNight :: [CardDef]
deadOfNight =
  [ headline
      "street-gangs-come-to-arkham"
      33
      "Street Gangs Come to Arkham"
      "Test (strength) and resolve the effect based on your test result:\n0: You become WANTED.\n1-2: You become WANTED and gain one common item.\n3+: You gain one common item."
      (graded Strength wanted (Seq [wanted, commonItem]) commonItem)
  , headline
      "all-roads-lead-to-arkham"
      34
      "All Roads Lead to Arkham!"
      "Test (observation) and resolve the effect based on your test result:\n0: You disengage all monsters and move to the unstable space.\n1-2: You disengage all monsters, move to the unstable space, and spawn one clue.\n3+: Spawn one clue."
      ( graded
          Observation
          (MoveDirectlyTo TheUnstableSpace)
          (Seq [MoveDirectlyTo TheUnstableSpace, SpawnOneClue])
          SpawnOneClue
      )
  , headline
      "strange-lights-on-strange-nights"
      35
      "Strange Lights on Strange Nights"
      "Test (lore) and resolve the effect based on your test result:\n0: You suffer two horror.\n1-2: You suffer two horror and gain one spell.\n3+: You gain one spell."
      (graded Lore (horror 2) (Seq [horror 2, spell]) spell)
  , headline
      "deputy-detects-dastardly-deeds"
      36
      "Deputy Detects Dastardly Deeds"
      "If there are one or more clues in your neighborhood, you become WANTED."
      (If (CountAtLeast CluesInYourNeighborhood 1) wanted NoEffect)
  , headline
      "planetary-convergence-nigh"
      37
      "Planetary Convergence Nigh"
      "Draw and resolve two additional tokens from the mythos cup."
      (DrawMythosTokens 2)
  , rumor
      "something-rotten-in-arkham"
      38
      "Something Rotten in Arkham"
      "Add this card to the codex and discard all other rumor headlines. Reckoning—Spawn one monster. Any investigator may suffer one damage and one horror to cancel this effect."
      (Just (Custom "rumor-something-rotten"))
  , rumorWith
      "stocks-stutter-as-banks-mutter"
      39
      "Stocks Stutter as Banks Mutter"
      "Add this card to the codex and discard all other rumor headlines. Discard the item in the display with the highest value. While this card is in the codex, reduce the size of the display by one card."
      (Custom "discard-richest-item")
      Nothing
  ]
