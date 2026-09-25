module AH3e.Content.Headlines (cards) where

import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill

headline :: CardCode -> Text -> Text -> Effect -> CardDef
headline code name txt eff = CardDef code name CoreSet 1 (HeadlineCard (HeadlineDef False txt eff Nothing))

rumor :: CardCode -> Text -> Text -> Maybe Effect -> CardDef
rumor code name txt reck = CardDef code name CoreSet 1 (HeadlineCard (HeadlineDef True txt NoEffect reck))

doomHere, spawnHere, spawnUnstable :: Effect
doomHere = PlaceDoomAt YourSpace (N 1)
spawnHere = SpawnMonsterIn YourSpace False
spawnUnstable = SpawnMonsterIn TheUnstableSpace False

graded :: Skill -> Effect -> Effect -> Effect -> Effect
graded sk zero oneTwo threePlus = Test sk 0 (ByResult [((1, Just 2), oneTwo), ((3, Nothing), threePlus)]) zero

unless' :: Text -> Effect -> Text -> Effect -> Effect
unless' sufferLabel suffer otherLabel other = Choose [(sufferLabel, suffer), (otherLabel, other)]

cards :: [CardDef]
cards =
  [ rumor
      "astronomers-chuffed-as-stars-align"
      "Astronomers Chuffed as Stars Align"
      "Add this card to the codex and discard all other rumor headlines. Reckoning—Each investigator tests (will). Each investigator who fails places one doom in their space. Then any investigator may spend one clue to discard this card."
      (Just (Custom "rumor-astronomers"))
  , headline
      "asylum-overflowing"
      "Asylum Overflowing"
      "You suffer one horror for each doom in your space."
      (SufferHorror (Counted DoomInYourSpace))
  , headline
      "banned-books-bandied-about"
      "Banned Books Bandied About"
      "For each clue you have, you suffer one horror unless you discard that clue."
      ( ForEachOf
          CluesYouHave
          (unless' "Suffer one horror" (horror 1) "Discard the clue" (Pay (SpendClues 1) NoEffect))
      )
  , headline
      "big-city-burglars-busted"
      "Big City Burglars Busted"
      "For each item you have, you suffer one damage unless you discard that item."
      (Custom "big-city-burglars-busted")
  , headline
      "break-in-at-historical-society"
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
      "Church Leaders Bless City"
      "Test (will) and resolve the effect based on your test result: 0: Spawn one monster in your space. 1–2: You become BLESSED. Spawn one monster in your space. 3+: You become BLESSED. (The monster engages you or another investigator in your space.)"
      (graded Will spawnHere (Seq [blessed, spawnHere]) blessed)
  , headline
      "concerned-citizens-congregate"
      "Concerned Citizens Congregate"
      "Test (influence) and resolve the effect based on your test result: 0: You place one doom in your space. 1–2: You gain one ally and place one doom in your space. 3+: You gain one ally. (If you are in a street space, place the doom in an adjacent neighborhood space instead.)"
      (graded Influence doomHere (Seq [ally, doomHere]) ally)
  , headline
      "curfew-established"
      "Curfew Established"
      "You suffer one damage for each monster in your neighborhood."
      (SufferDamage (Counted MonstersInYourNeighborhood))
  , headline
      "cursed-treasure-brings-money-problems"
      "Cursed Treasure Brings Money Problems"
      "Test (will) and resolve the effect based on your test result: 0: You become CURSED. 1–2: You gain $3 and become CURSED. 3+: You gain $3."
      (graded Will cursed (Seq [money 3, cursed]) (money 3))
  , headline
      "dream-expert-found-dead"
      "Dream Expert Found Dead"
      "Test (will). You suffer three horror; prevent one horror for each success you rolled."
      (let e = SufferHorror (Diff (N 3) TestResult) in Test Will 0 e e)
  , headline
      "foul-footprints-found"
      "Foul Footprints Found"
      "Test (will) and resolve the effect based on your test result: 0: Spawn one monster at the unstable space. 1–2: You gain one remnant. Spawn one monster at the unstable space. 3+: You gain one remnant."
      (graded Will spawnUnstable (Seq [remnants 1, spawnUnstable]) (remnants 1))
  , rumor
      "full-moon-linked-to-aberrant-behavior"
      "Full Moon Linked to Aberrant Behavior"
      "Add this card to the codex and discard all other rumor headlines. Reckoning—Each investigator tests (will). Each investigator who fails suffers two horror. Then any investigator may spend one clue to discard this card."
      (Just (Custom "rumor-full-moon"))
  , headline
      "its-curtains-for-arkham"
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
      "Magician Vanishes Rabbit, Self"
      "You disengage all monsters and move to the unstable space. (The monsters are not exhausted. They engage another investigator in their space.)"
      (Custom "magician-vanishes")
  , headline
      "masked-man-mystery"
      "Masked Man Mystery"
      "Choose another investigator in any space. Spawn one monster in that investigator's space. (It engages that investigator or another investigator in that space.)"
      (Custom "masked-man-mystery")
  , headline
      "miskatonic-museum-mystery"
      "Miskatonic Museum Mystery"
      "Test (will) and resolve the effect based on your test result: 0: You become CURSED. 1–2: You gain one curio item and become CURSED. 3+: You gain one curio item."
      (graded Will cursed (Seq [curioItem, cursed]) curioItem)
  , headline
      "mysticism-malfeasance"
      "Mysticism Malfeasance"
      "Test (will −1). If you fail, place one doom in your space. (If you are in a street space, place the doom in an adjacent neighborhood space instead.)"
      (Test Will (-1) NoEffect doomHere)
  , headline
      "night-terrors-terrify-nightly"
      "Night Terrors Terrify Nightly"
      "Test (will). You suffer three damage; prevent one damage for each success you rolled."
      (let e = SufferDamage (Diff (N 3) TestResult) in Test Will 0 e e)
  , headline
      "northside-vigilante-apprehended"
      "Northside Vigilante Apprehended"
      "You choose one: • You suffer two damage and two horror. • You become CURSED."
      (Choose [("Suffer two damage and two horror", harm 2 2), ("Become CURSED", cursed)])
  , headline
      "occult-activity-threatens-city"
      "Occult Activity Threatens City"
      "You choose one: • Place one doom in your space. (If you are in a street space, place the doom in an adjacent neighborhood space instead.) • Spawn one monster in your space. (It engages you or another investigator in your space.)"
      (Choose [("Place one doom in your space", doomHere), ("Spawn one monster in your space", spawnHere)])
  , headline
      "police-do-your-jobs"
      "Police, Do Your Jobs!"
      "For each clue in your neighborhood you suffer one damage or one horror."
      ( ForEachOf
          CluesInYourNeighborhood
          (Choose [("Suffer one damage", damage 1), ("Suffer one horror", horror 1)])
      )
  , headline
      "purse-snatchers-pursued"
      "Purse Snatchers Pursued"
      "You suffer two damage unless you discard one item."
      (unless' "Suffer two damage" (damage 2) "Discard one item" (Pay (CostDiscard ItemCard) NoEffect))
  , rumor
      "rogue-comet-approaches"
      "Rogue Comet Approaches"
      "Add this card to the codex and discard all other rumor headlines. Reckoning—Any investigator may spend one clue to discard this card. Otherwise, place one doom on this card. When there is three doom on this card, resolve a gate burst mythos effect. Then, discard this card."
      (Just (Custom "rumor-comet"))
  , headline
      "so-called-pharaoh-speaks-in-cambridge"
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
      "Starstruck Scientists Stumped"
      "For each spell you have, you suffer one horror unless you discard that spell."
      ( ForEachOf
          SpellsYouHave
          (unless' "Suffer one horror" (horror 1) "Discard the spell" (Pay (CostDiscard SpellCard) NoEffect))
      )
  , headline
      "symposium-snafu"
      "Symposium Snafu"
      "If you have one or more clues, you become CURSED."
      (If (HasClues 1) cursed NoEffect)
  , rumor
      "truckers-strike-leads-to-shortages"
      "Truckers' Strike Leads to Shortages"
      "Add this card to the codex and discard all other rumor headlines. The value of each item in the display is increased by two for as long as this card is in the codex. Before you would buy one or more items from the display, you may test (influence −1). If you pass, you ignore this card this round."
      Nothing
  , headline
      "unexplained-sightings-near-river"
      "Unexplained Sightings Near River"
      "You suffer one damage for each doom in your space."
      (SufferDamage (Counted DoomInYourSpace))
  , headline
      "unscheduled-parade-shuts-down-main-st"
      "Unscheduled Parade Shuts Down Main St."
      "You suffer one horror for each monster in your neighborhood."
      (SufferHorror (Counted MonstersInYourNeighborhood))
  , headline
      "we-have-no-one-to-blame-but-ourselves"
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
      "Why Even Go On Anymore?"
      "You roll one die. (This die roll is not a test.) You suffer a total amount of damage and/or horror equal to the die result. (You choose how to split the die result between damage and horror.)"
      (Custom "why-even-go-on")
  , headline
      "wild-animal-attacks-spike"
      "Wild Animal Attacks Spike"
      "Choose one non-epic monster in any space. You suffer damage equal to that monster's remaining health. Then you defeat it."
      (Custom "wild-animal-attacks")
  ]
