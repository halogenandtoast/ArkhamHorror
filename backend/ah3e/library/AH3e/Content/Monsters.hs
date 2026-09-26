{- | Monster cards.

'cards' holds the monsters whose ready and exhausted faces have both been
transcribed. 'roster' records the name, traits and physical copy count of
every monster whose art we have, including the ones still to be transcribed,
so scenario decks can be built with the right counts.
-}
module AH3e.Content.Monsters (cards) where

import AH3e.Content.Tiles (spaceIdFor)
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill

{- | A monster that moves "directly to" its prey rather than a fixed number of
spaces; the engine walks the whole path, so any distance past the board's
diameter behaves the same.
-}
directly :: Int
directly = 99

{- | Named cultists with no spawn text; they are placed by scenario or event
effects rather than drawn from the monster deck.
-}
noSpawnText :: SpaceRule
noSpawnText = CustomSpaceRule "no spawn text"

{- | The monsters that leave a remnant behind when they are defeated (429.9). The
cultists and the human enemies leave nothing, so the list is shorter than the deck.
-}
remnantMonsters :: [CardCode]
remnantMonsters =
  [ "abyssal-servant"
  , "altered-beast"
  , "altered-servant"
  , "avian-thrall"
  , "cantor-of-rlyeh"
  , "capricious-stalker"
  , "cerebral-extractor"
  , "confounding-specter"
  , "corben-bouchard"
  , "corpse-taker"
  , "coursing-hound"
  , "crashing-specter"
  , "crawling-one"
  , "crazed-fiend"
  , "creeping-ghoul"
  , "cruel-slaver"
  , "dread-shadow"
  , "entranced-hybrid"
  , "eyeless-watcher"
  , "feasting-master"
  , "flesh-eater"
  , "frenzied-hunter"
  , "ghoul-acolyte"
  , "ghoul-priest"
  , "gluttonous-giant"
  , "grasping-fungus"
  , "guardian-beast"
  , "hovering-byakhee"
  , "hulking-thrall"
  , "icebound-captive"
  , "keening-hound"
  , "lodge-seer"
  , "lupine-thrall"
  , "menacing-bulk"
  , "morphic-terror"
  , "nightmarish-fiend"
  , "ocean-scion"
  , "pale-lord"
  , "ravenous-predator"
  , "river-skulk"
  , "rlyeh-guardian"
  , "sanguinous-wraith"
  , "sea-singer"
  , "shallows-predator"
  , "shoreline-brute"
  , "siobhan-riley"
  , "swift-byakhee"
  , "swooping-scavenger"
  , "taloned-cannibal"
  , "tindalos-alpha"
  , "tunneling-dhole"
  , "twilight-sentry"
  , "undulating-mass"
  , "vicious-glutton"
  , "void-touched"
  , "vomitous-wraith"
  , "wake-titan"
  ]

monster
  :: CardCode
  -> Text
  -> Int
  -- ^ copies
  -> [Trait]
  -> Int
  -- ^ speed
  -> SpaceRule
  -> Activation
  -> (Int, Int)
  -- ^ health, elite
  -> (Int, Int)
  -- ^ attack modifier, evade modifier
  -> (Int, Int)
  -- ^ damage, horror
  -> [Keyword]
  -> Text
  -> CardDef
monster code name copies traits speed spawn activation health mods harm keywords =
  monsterWith
    code
    name
    copies
    traits
    speed
    spawn
    activation
    health
    mods
    harm
    keywords
    Strength
    Nothing

{- | A monster whose attack tests a skill other than strength, or whose ready
face carries a different name (several cards share one ready face).
-}
monsterWith
  :: CardCode
  -> Text
  -> Int
  -> [Trait]
  -> Int
  -> SpaceRule
  -> Activation
  -> (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> [Keyword]
  -> Skill
  -> Maybe Text
  -> Text
  -> CardDef
monsterWith code name copies traits speed spawn activation (health, elite) (atk, evade) (damage, horror) keywords attackSkill readyName text =
  CardDef
    code
    name
    CoreSet
    copies
    ( MonsterCard
        MonsterDef
          { readyName
          , spawn
          , activation
          , speed
          , traits
          , health
          , elite
          , attackSkill
          , attackModifier = atk
          , evadeModifier = evade
          , damage
          , horror
          , remnant = code `elem` remnantMonsters
          , keywords
          , epic = False
          , text
          }
    )

cards :: [CardDef]
cards =
  [ monster
      "abyssal-servant"
      "Abyssal Servant"
      1
      ["Nightgaunt"]
      2
      (NearestStreetTo Nothing)
      (Hunter MostClues)
      (4, 0)
      (-1, -2)
      (1, 2)
      []
      "After you fail to evade this monster, you disengage other monsters and both you and it move one space toward the unstable space."
  , monster
      "eyeless-watcher"
      "Eyeless Watcher"
      1
      ["Nightgaunt"]
      2
      UnstableSpace
      (Hunter (LowestSkill Influence))
      (3, 0)
      (0, -2)
      (0, 1)
      [Watcher]
      "Watcher (This monster does not restrict your actions or encounters. It moves with you.)"
  , monster
      "high-priest"
      "High Priest"
      1
      ["Human", "Cultist"]
      0
      MostDoomSpace
      (Lurker (PlaceDoomAt SourceSpace (N 1)))
      (2, 0)
      (-1, -1)
      (1, 0)
      []
      "Reward-After you defeat this monster as part of an attack action, you gain one spell."
  , monster
      "hooded-stalker"
      "Hooded Stalker"
      2
      ["Human", "Cultist"]
      2
      MostDoomSpace
      (Hunter (LowestSkill Strength))
      (1, 0)
      (0, -1)
      (1, 0)
      []
      "\"Hello, friend. What are you doing out here so late at night, and all alone, too?\""
  , monster
      "occult-ritualist"
      "Occult Ritualist"
      2
      ["Human", "Cultist"]
      0
      (NearestStreetTo (Just TheLeader))
      (Lurker (PlaceDoomAt AdjacentSpace (N 1)))
      (1, 0)
      (0, 1)
      (0, 1)
      [Elusive]
      "Elusive (This monster does not engage investigators in its space.) The incense smells like sweet, sticky summer days and rotting meat."
  , monster
      "robed-figure"
      "Robed Figure"
      3
      ["Human", "Cultist"]
      2
      UnstableSpace
      (Patrol UnstableSpace (Just (HighestSkill Influence)))
      (1, 0)
      (0, 0)
      (1, 0)
      []
      "The long robes and deep hood throw the figure's face into darkness, but the hand that reaches for you seems to shine in the moonlight."
  , monster
      "swift-byakhee"
      "Swift Byakhee"
      1
      ["Byakhee"]
      3
      MostDoomSpace
      (Hunter (LowestSkill Observation))
      (3, 0)
      (0, -1)
      (1, 1)
      []
      "Reward-After you defeat this monster as part of an attack action, you may disengage all monsters and move up to three spaces."
  , monster
      "tindalos-alpha"
      "Tindalos Alpha"
      1
      ["Hound of Tindalos"]
      directly
      MostDoomSpace
      (Hunter (HighestSkill Observation))
      (2, 1)
      (-2, -2)
      (2, 2)
      []
      "Elite 1 (Has 1 additional health per investigator.) With a twist, the creature vanishes into a single point. With a crack, it appears again, right behind you."
  , monster
      "ravenous-predator"
      "Ravenous Predator"
      1
      ["Hound of Tindalos"]
      directly
      UnstableSpace
      (Hunter (LowestSkill Will))
      (3, 0)
      (-1, -1)
      (1, 1)
      []
      "\"All the evil in the universe was concentrated in their lean, hungry bodies.\" -The Hounds of Tindalos, Frank Belknap Long"
  , monster
      "keening-hound"
      "Keening Hound"
      1
      ["Hound of Tindalos"]
      directly
      MostDoomSpace
      (Hunter MostRemnants)
      (4, 0)
      (-1, -2)
      (2, 1)
      []
      "A rusty shriek. A billow of blue smoke. Then a lean, hungry, not-quite-real monstrosity comes twisting out of the corner, teeth-first..."
  , monster
      "coursing-hound"
      "Coursing Hound"
      1
      ["Hound of Tindalos"]
      directly
      UnstableSpace
      (Hunter (HighestSkill Strength))
      (3, 0)
      (-2, -1)
      (1, 2)
      [Retaliate]
      "Retaliate (After you perform an attack action, if you did not damage this monster, it attacks you.)"
  , monster
      "simon-carter"
      "Simon Carter"
      1
      ["Lodge", "Human"]
      0
      (NearestStreetTo (Just TheLeader))
      (Lurker (PlaceDoomAt AdjacentSpaceWithMostDoom (N 1)))
      (2, 1)
      (-1, -1)
      (1, 1)
      []
      "Elite 1 (Has 1 additional health per investigator.) \"If my life, or yours, is the price we must pay, then so be it.\""
  , monster
      "shoreline-brute"
      "Shoreline Brute"
      1
      ["Deep One"]
      2
      UnstableSpace
      (Hunter (HighestSkill Will))
      (4, 0)
      (-2, 0)
      (2, 1)
      []
      "\"I saw them in a limitless stream-flopping, hopping, croaking, bleating.\" -The Shadow over Innsmouth, H.P. Lovecraft"
  , monster
      "sea-singer"
      "Sea Singer"
      1
      ["Deep One"]
      0
      MostDoomSpace
      (Lurker (Seq [RemoveDoomFrom SourceSpace (N 1), PlaceDoomAt ScenarioSheet (N 1)]))
      (3, 0)
      (-1, -1)
      (1, 2)
      []
      "The loathsome toad-creature opens its fang-filled mouth and sings a strange, high music that makes you dream of the endless sea."
  , monster
      "ruth-turner"
      "Ruth Turner"
      1
      ["Human", "Cultist"]
      0
      noSpawnText
      (Lurker (PlaceDoomAt (TheSpace (spaceIdFor "Hangman's Hill")) (N 1)))
      (3, 0)
      (0, -2)
      (1, 0)
      [Elusive]
      "Elusive (Ruth does not engage investigators in her space.) After you disengage Ruth, she is defeated."
  , monster
      "wake-titan"
      "Wake Titan"
      1
      ["Deep One"]
      2
      UnstableSpace
      (Patrol UnstableSpace Nothing)
      (2, 1)
      (-2, 0)
      (2, 2)
      [Massive]
      "Elite 1 (Has 1 additional health per investigator.) Massive (This monster engages and attacks all investigators in its space. It cannot be exhausted.)"
  , monster
      "wolf-man-drew"
      "\"Wolf-Man\" Drew"
      1
      ["Human", "Cultist"]
      2
      noSpawnText
      (Hunter (LowestSkill Strength))
      (3, 0)
      (-1, 0)
      (2, 0)
      [Feed]
      "Feed (After Drew deals damage to an investigator or ally, he recovers that much health.)"
  , monster
      "lupine-thrall"
      "Lupine Thrall"
      1
      ["Thrall"]
      2
      (NearestStreetTo Nothing)
      (Hunter (LowestSkill Will))
      (4, 0)
      (0, -2)
      (2, 1)
      []
      "As part of an evade action, you may spend one remnant to add one to the result of one die."
  , monster
      "rlyeh-guardian"
      "R'lyeh Guardian"
      1
      ["Star Spawn"]
      1
      MostDoomSpace
      (Patrol MostDoomSpace Nothing)
      (4, 1)
      (-2, -1)
      (3, 2)
      [Massive]
      "Elite 1 (Has 1 additional health per investigator.) Massive (This monster engages and attacks all investigators in its space. It cannot be exhausted.)"
  , monster
      "nightmarish-fiend"
      "Nightmarish Fiend"
      1
      ["Ghoul"]
      2
      (NearestStreetTo Nothing)
      (Hunter (LowestSkill Will))
      (4, 0)
      (-1, -1)
      (2, 2)
      [Feed]
      "Feed (After this monster deals damage to an investigator or ally, it recovers that much health.)"
  , monster
      "masked-hunter"
      "Masked Hunter"
      1
      ["Human", "Cultist"]
      2
      noSpawnText
      (Hunter MostClues)
      (2, 1)
      (-2, 0)
      (2, 1)
      []
      "Elite 1 (Has 1 additional health per investigator.) He never speaks, never slows, never stops, never hesitates. He only hunts. And he's closing in."
  , monster
      "masked-ones"
      "Masked Ones"
      1
      ["Human", "Cultist"]
      0
      MostDoomSpace
      (Lurker (PlaceDoomAt SourceSpace (N 1)))
      (2, 1)
      (0, -2)
      (0, 1)
      []
      "Elite 1 (Has 1 additional health per investigator.) After this monster attacks, you suffer two damage unless you place one doom in your space."
  , monster
      "corpse-taker"
      "Corpse-Taker"
      1
      ["Aberration"]
      0
      MostDoomSpace
      (Lurker (Seq [RemoveDoomFrom SourceSpace (N 1), PlaceDoomAt ScenarioSheet (N 1)]))
      (2, 1)
      (-2, -1)
      (2, 2)
      []
      "Elite 1 (Has 1 additional health per investigator.)"
  , monster
      "ghoul-priest"
      "Ghoul Priest"
      1
      ["Ghoul", "Cultist"]
      0
      MostDoomSpace
      (Lurker (ForInvestigators EveryInvestigator (SufferDamage (N 1))))
      (4, 0)
      (-1, -1)
      (2, 1)
      []
      "Reward-After you defeat this monster as part of an attack action, you gain one curio item."
  , monster
      "herman-collins"
      "Herman Collins"
      1
      ["Human", "Cultist"]
      0
      noSpawnText
      (Lurker (ForInvestigators EveryInvestigator DiscardAFocus))
      (3, 0)
      (-1, -1)
      (1, 1)
      []
      "After you perform a focus action while engaged with Herman, you may spend two focus to defeat him."
  , monster
      "avian-thrall"
      "Avian Thrall"
      1
      ["Thrall"]
      3
      MostDoomSpace
      (Hunter MostClues)
      (3, 0)
      (-2, -1)
      (1, 2)
      []
      "The horde of white-eyed pigeons stares at you, murmuring and cooing. And then a terrible shadow falls upon you."
  , monster
      "billy-cooper"
      "Billy Cooper"
      1
      ["Human", "Cultist"]
      0
      noSpawnText
      (Lurker (PlaceDoomAt (TheSpace (spaceIdFor "Police Station")) (N 1)))
      (3, 0)
      (-1, -1)
      (2, 0)
      []
      "If you defeat another monster while engaged with Billy, he is also defeated."
  , monster
      "alma-hill"
      "Alma Hill"
      1
      ["Human", "Cultist"]
      0
      noSpawnText
      (Lurker (DrawMythosTokens 2))
      (3, 0)
      (0, -1)
      (0, 2)
      [Elusive]
      "Elusive (Alma does not engage investigators in her space.) After you defeat Alma, test lore. If you fail, you draw two tokens from the mythos cup."
  , monster
      "creeping-ghoul"
      "Creeping Ghoul"
      1
      ["Ghoul"]
      2
      UnstableSpace
      (Patrol UnstableSpace (Just (HighestSkill Lore)))
      (3, 0)
      (-1, 0)
      (2, 1)
      []
      "Something moves in the shadows. There's a crash and a low growl, and then a cat streaks past you. Just a scared cat... and the stink of the grave."
  , monster
      "whippoorwill"
      "Whippoorwill"
      2
      ["Beast"]
      1
      UnstableSpace
      (Hunter NearestInvestigator)
      (1, 0)
      (0, -1)
      (0, 0)
      [Watcher]
      "Watcher (This monster does not restrict your actions or encounters. It moves with you.) You roll one fewer die while resolving tests."
  , monster
      "vicious-glutton"
      "Vicious Glutton"
      2
      ["Ghoul"]
      2
      UnstableSpace
      (Patrol MostDoomSpace (Just (LowestSkill Will)))
      (2, 0)
      (-1, 1)
      (1, 1)
      []
      "The creature looks up, still licking clotted blood from its white fangs."
  , monster
      "hulking-thrall"
      "Hulking Thrall"
      2
      ["Human", "Thrall"]
      2
      (NearestStreetTo Nothing)
      (Hunter (HighestSkill Observation))
      (2, 0)
      (0, 0)
      (1, 1)
      []
      "After this monster attacks, you become CURSED unless you spend one remnant."
  , monster
      "lodge-enforcer"
      "Lodge Enforcer"
      2
      ["Lodge", "Human"]
      2
      UnstableSpace
      (Hunter (HighestSkill Lore))
      (1, 0)
      (0, -1)
      (2, 0)
      []
      "After you disengage this monster, you suffer one horror. \"You don't understand. We have a higher purpose. I can't allow you to stand in our way.\""
  , monster
      "ghoul-acolyte"
      "Ghoul Acolyte"
      2
      ["Ghoul", "Cultist"]
      0
      UnstableSpace
      (Lurker (PlaceDoomAt TheUnstableSpace (N 1)))
      (2, 0)
      (0, -1)
      (1, 1)
      []
      "After you evade this monster, it moves one space toward the unstable space."
  , monster
      "hybrid-thug"
      "Hybrid Thug"
      2
      ["Human", "Deep One"]
      2
      UnstableSpace
      (Patrol MostDoomSpace (Just (LowestSkill Observation)))
      (1, 0)
      (0, 0)
      (1, 0)
      []
      "After you defeat this monster as part of an attack action, you suffer one horror."
  , monster
      "ocean-scion"
      "Ocean Scion"
      2
      ["Deep One"]
      2
      MostDoomSpace
      (Patrol MostDoomSpace (Just (LowestSkill Influence)))
      (2, 0)
      (-1, -1)
      (1, 1)
      []
      "Footsteps squelch closer, and the salty smell of sun-baked seaweed washes over you. Teeth shine in the dark."
  , monster
      "shallows-predator"
      "Shallows Predator"
      2
      ["Deep One"]
      2
      (NearestStreetTo Nothing)
      (Hunter (HighestSkill Observation))
      (3, 0)
      (-1, 1)
      (1, 1)
      []
      "The salty scent of the sea breaks over you, but it is the bitter tang of blood that smells the strongest."
  , monster
      "void-touched"
      "Void Touched"
      2
      ["Human", "Thrall"]
      0
      (NearestStreetTo (Just TheLeader))
      (Lurker (ForInvestigators EveryInvestigator (SufferHorror (N 1))))
      (2, 0)
      (0, 0)
      (0, 2)
      []
      "Reward-After you defeat this monster as part of an attack action, you gain one spell. It sings with a thousand alien voices that echo in your mind."
  , monster
      "twilight-supplicant"
      "Twilight Supplicant"
      2
      ["Lodge", "Human"]
      0
      MostDoomSpace
      (Lurker (PlaceDoomAt SourceSpace (N 1)))
      (1, 0)
      (0, 0)
      (0, 2)
      [Elusive]
      "Elusive (This monster does not engage investigators in its space.) \"Yog-Sothoth knows the gate. Yog-Sothoth is the gate.\""
  , monster
      "altered-servant"
      "Altered Servant"
      2
      ["Human", "Thrall"]
      2
      UnstableSpace
      (Patrol UnstableSpace (Just MostClues))
      (2, 0)
      (0, -1)
      (1, 1)
      []
      "At first, the man seems normal as he trudges closer. But then the terrible burden on his back twitches."
  , monster
      "altered-beast"
      "Altered Beast"
      2
      ["Aberration"]
      2
      MostDoomSpace
      (Patrol MostDoomSpace (Just (LowestSkill Will)))
      (1, 0)
      (0, 0)
      (1, 1)
      []
      "After this monster attacks, you discard one focus."
  , monster
      "flesh-eater"
      "Flesh-Eater"
      2
      ["Ghoul"]
      2
      UnstableSpace
      (Hunter (HighestSkill Strength))
      (2, 0)
      (0, 0)
      (1, 1)
      []
      "There's a wet crunching sound and the thing's shoulders quiver as it tears flesh from the bone."
  , monster
      "river-skulk"
      "River Skulk"
      2
      ["Deep One"]
      0
      (NearestStreetTo (Just TheLeader))
      (Lurker (ForInvestigators EveryInvestigator (SufferHorror (N 1))))
      (3, 0)
      (0, -1)
      (1, 2)
      []
      "\"Their predominant colour was a greyish-green, though they had white bellies.\" -The Shadow Over Innsmouth, H.P. Lovecraft"
  , monster
      "lodge-loyalist"
      "Lodge Loyalist"
      2
      ["Lodge", "Human"]
      2
      MostDoomSpace
      (Patrol MostDoomSpace (Just MostSpells))
      (1, 0)
      (-1, 0)
      (1, 1)
      []
      "After you disengage this monster, place one doom in this space. \"The Lodge has entrusted me to see this done. For the good of us all.\""
  , monster
      "accursed-somnambulist"
      "Accursed Somnambulist"
      2
      ["Dreaming", "Human"]
      0
      UnstableSpace
      (Lurker (PlaceDoomAt SourceSpace (N 1)))
      (2, 0)
      (0, -1)
      (0, 2)
      []
      "After you defeat this monster as part of an attack action, test will. If you fail, you become CURSED."
  , monster
      "cantor-of-rlyeh"
      "Cantor of R'lyeh"
      1
      ["Star Spawn"]
      0
      MostDoomSpace
      (Lurker (DrawMythosTokens 1))
      (4, 1)
      (-1, -2)
      (2, 2)
      [Massive]
      "Elite 1 (Has 1 additional health per investigator.) Massive (This monster engages and attacks all investigators in its space. It cannot be exhausted.)"
  , monster
      "cruel-slaver"
      "Cruel Slaver"
      1
      ["Moon-Beast"]
      0
      UnstableSpace
      (Lurker (Seq [RemoveDoomFrom SourceSpace (N 1), PlaceDoomAt ScenarioSheet (N 1)]))
      (4, 0)
      (-1, -1)
      (1, 2)
      [Retaliate]
      "Retaliate (After you perform an attack action, if you did not damage this monster, it attacks you.)"
  , monster
      "declan-pearce"
      "Declan Pearce"
      1
      ["Human", "Cultist"]
      directly
      UnstableSpace
      (Patrol UnstableSpace Nothing)
      (3, 0)
      (-2, 1)
      (1, 1)
      [Elusive]
      "Elusive. After you perform an attack action, Declan disengages you unless you spend one focus. (He does not exhaust.)"
  , monster
      "dread-shadow"
      "Dread Shadow"
      1
      ["Shantak Servitor"]
      2
      UnstableSpace
      (Hunter (HighestSkill Lore))
      (2, 1)
      (-1, -1)
      (1, 2)
      [Massive, Pursuit]
      "Elite 1. Massive. Pursuit (After this monster is dealt damage by an investigator in another space, it moves toward that investigator.)"
  , monster
      "enraged-dreamer"
      "Enraged Dreamer"
      2
      ["Dreaming", "Human"]
      1
      MostDoomSpace
      (Hunter (LowestSkill Will))
      (2, 0)
      (-1, 0)
      (1, 0)
      []
      "After you defeat this monster as part of an attack action, you suffer one horror."
  , monster
      "entranced-hybrid"
      "Entranced Hybrid"
      1
      ["Dreaming", "Deep One"]
      0
      UnstableSpace
      (Lurker (PlaceDoomAt TheUnstableSpace (N 1)))
      (2, 0)
      (-1, 0)
      (1, 1)
      []
      "After this monster attacks, move one doom from its space to the scenario sheet."
  , monster
      "feasting-master"
      "Feasting Master"
      1
      ["Moon-Beast"]
      2
      MostDoomSpace
      (Hunter MostAllies)
      (2, 1)
      (1, 0)
      (2, 2)
      [Feed]
      "Elite 1 (Has one additional health per investigator.) Feed (After this monster deals damage to an investigator or ally, it recovers that much health.)"
  , monster
      "frenzied-hunter"
      "Frenzied Hunter"
      1
      ["Deep One"]
      2
      UnstableSpace
      (Hunter MostDamage)
      (3, 0)
      (-1, -1)
      (2, 1)
      [Relentless]
      "Relentless (This monster cannot be exhausted and can only be damaged by an investigator in its space.)"
  , monster
      "guardian-beast"
      "Guardian Beast"
      1
      ["Shantak Servitor"]
      3
      MostDoomSpace
      (Patrol UnstableSpace (Just (HighestSkill Strength)))
      (2, 1)
      (-1, 0)
      (2, 1)
      [Massive, Retaliate]
      "Elite 1. Massive. Retaliate (After you perform an attack action, if you did not damage this monster, it attacks you.)"
  , monster
      "hovering-byakhee"
      "Hovering Byakhee"
      1
      ["Byakhee"]
      2
      MostDoomSpace
      (Lurker (PlaceDoomAt SourceSpace (N 1)))
      (3, 0)
      (-1, 0)
      (2, 1)
      [Pursuit]
      "Pursuit (After this monster is dealt damage by an investigator in another space, it moves toward that investigator.)"
  , monster
      "icebound-captive"
      "Icebound Captive"
      2
      ["Human", "Thrall"]
      0
      (NearestStreetTo (Just TheLeader))
      (Lurker (ForInvestigators EveryInvestigator (SufferDamage (N 1))))
      (2, 0)
      (0, 0)
      (2, 0)
      []
      "Reward-After you defeat this monster as part of an attack action, you gain an ally."
  , monster
      "pale-lord"
      "Pale Lord"
      1
      ["Moon-Beast"]
      0
      (NearestStreetTo (Just TheLeader))
      (Lurker (PlaceDoomAt TheUnstableSpace (N 1)))
      (4, 0)
      (-2, 0)
      (1, 1)
      [Elusive]
      "Elusive (This monster does not engage investigators in its space.) After this monster attacks, place one doom in its space."
  , monster
      "prowling-abductor"
      "Prowling Abductor"
      2
      ["Human", "Cultist"]
      2
      (NearestStreetTo Nothing)
      (Hunter (LowestSkill Influence))
      (1, 0)
      (-1, -1)
      (1, 0)
      [Relentless]
      "After this monster attacks, you become delayed. Relentless (This monster cannot be exhausted and can only be damaged by an investigator in its space.)"
  , monster
      "swooping-scavenger"
      "Swooping Scavenger"
      1
      ["Byakhee"]
      2
      UnstableSpace
      (Patrol MostDoomSpace (Just (LowestSkill Strength)))
      (3, 0)
      (-1, 0)
      (1, 1)
      []
      "After this monster attacks, you disengage other monsters and both you and it move directly to the space with the most doom."
  , monster
      "terrified-wanderer"
      "Terrified Wanderer"
      2
      ["Dreaming", "Human"]
      2
      (NearestStreetTo (Just TheLeader))
      (Hunter (LowestSkill Will))
      (1, 0)
      (-1, 1)
      (1, 1)
      []
      "After this monster becomes engaged with you, test influence -1. If you pass, defeat it. If you fail, you suffer one damage."
  , monsterWith
      "brawling-riot"
      "Brawling Riot"
      1
      ["Human", "Mob"]
      0
      UnstableSpace
      (Lurker (PlaceDoomAt SourceSpace (N 1)))
      (2, 1)
      (-1, 1)
      (1, 1)
      [Massive]
      Strength
      Nothing
      "Elite 1 (Has 1 additional health per investigator.) Massive (This monster engages and attacks each investigator in its space. It cannot be exhausted.)"
  , monsterWith
      "brutal-goons"
      "Brutal Goons"
      2
      ["Sheldon", "Human"]
      2
      (NearestStreetTo Nothing)
      (Hunter MostMoney)
      (4, 0)
      (-1, -1)
      (1, 0)
      [Massive]
      Strength
      Nothing
      "Massive (This monster engages and attacks each investigator in its space. It cannot be exhausted.)"
  , monsterWith
      "capricious-stalker"
      "Capricious Stalker"
      1
      ["Nightgaunt"]
      2
      UnstableSpace
      (Hunter LeastDamage)
      (3, 0)
      (0, -2)
      (1, 1)
      []
      Strength
      Nothing
      "After this monster attacks, it disengages you and moves one space toward the investigator with the least damage. Like a cat with a mouse."
  , monsterWith
      "cerebral-extractor"
      "Cerebral Extractor"
      1
      ["Mi-Go"]
      0
      UnstableSpace
      (Lurker (Custom "return-spawn-monster-token"))
      (4, 0)
      (-1, -1)
      (1, 1)
      []
      Strength
      Nothing
      "Reward-After you defeat this monster as part of an attack action, return it to the box and gain the MI-GO BRAINCASE."
  , monsterWith
      "corben-bouchard"
      "Corben Bouchard"
      1
      ["Sheldon", "Servitor"]
      2
      (NearestStreetTo Nothing)
      (Hunter (HighestSkill Strength))
      (2, 1)
      (-1, -1)
      (1, 1)
      []
      Strength
      Nothing
      "Elite 1 (Has 1 additional health per investigator.) After you defeat this monster as part of an attack action, suffer one damage."
  , monsterWith
      "crawling-one"
      "Crawling One"
      2
      ["Aberration"]
      1
      MostDoomSpace
      (Hunter (LowestSkill Will))
      (3, 0)
      (-1, 1)
      (1, 1)
      []
      Strength
      Nothing
      "After you fail to evade this monster, you suffer one horror. The writhing mass of flesh shudders as it bears down upon you."
  , monsterWith
      "crazed-fiend"
      "Crazed Fiend"
      1
      ["Ghast"]
      2
      MostDoomSpace
      (Patrol MostDoomSpace (Just (LowestSkill Will)))
      (3, 0)
      (-1, -2)
      (2, 1)
      [Feed]
      Strength
      Nothing
      "Feed (After this monster deals damage to an investigator or ally, it recovers that much health.)"
  , monsterWith
      "feckless-agitator"
      "Feckless Agitator"
      1
      ["O'Bannion", "Human"]
      0
      UnstableSpace
      (Lurker (PlaceDoomAt SourceSpace (N 1)))
      (1, 0)
      (0, 0)
      (1, 0)
      [Elusive]
      Strength
      Nothing
      "Elusive (This monster does not engage investigators in its space.)"
  , monsterWith
      "gluttonous-giant"
      "Gluttonous Giant"
      1
      ["Gug"]
      2
      UnstableSpace
      (Patrol MostDoomSpace (Just (LowestSkill Strength)))
      (2, 1)
      (-2, -1)
      (2, 2)
      [Massive]
      Strength
      Nothing
      "Elite 1. Massive. After you deal damage to this monster as part of an attack action, suffer one horror (even if you defeat it)."
  , monsterWith
      "grasping-fungus"
      "Grasping Fungus"
      2
      ["Mi-Go"]
      3
      MostDoomSpace
      (Patrol MostDoomSpace (Just MostItems))
      (2, 0)
      (-1, 0)
      (1, 1)
      []
      Strength
      Nothing
      "Damage dealt by this monster must be assigned to items, if able."
  , monsterWith
      "hit-squad"
      "Hit Squad"
      2
      ["O'Bannion", "Human"]
      2
      UnstableSpace
      (Hunter MostClues)
      (4, 0)
      (-1, -1)
      (1, 0)
      [Massive]
      Strength
      Nothing
      "Massive (This monster engages and attacks each investigator in its space. It cannot be exhausted.)"
  , monsterWith
      "lodge-guardian"
      "Lodge Guardian"
      1
      ["Lodge", "Human"]
      2
      UnstableSpace
      (Patrol MostDoomSpace (Just MostSpells))
      (2, 0)
      (-1, -1)
      (1, 1)
      [Retaliate]
      Strength
      Nothing
      "Retaliate (After you perform an attack action, if you did not damage this monster, it attacks you.) After you disengage this monster, become CURSED."
  , monsterWith
      "lodge-seer"
      "Lodge Seer"
      1
      ["Lodge", "Human"]
      2
      (NearestStreetTo (Just TheLeader))
      (Lurker (PlaceDoomAt TheUnstableSpace (N 1)))
      (2, 0)
      (0, -2)
      (1, 2)
      []
      Lore
      Nothing
      "After you disengage this monster, place two doom at the unstable space."
  , monsterWith
      "menacing-bulk"
      "Menacing Bulk"
      2
      ["Gug"]
      0
      (NearestStreetTo (Just TheLeader))
      (Lurker (ForInvestigators NearestToSource (SufferHorror (N 1))))
      (3, 0)
      (-2, -1)
      (1, 1)
      []
      Strength
      Nothing
      "After this monster attacks you, become FATIGUED. If you cannot, suffer one additional damage."
  , monsterWith
      "mob-enforcer"
      "Mob Enforcer"
      2
      ["O'Bannion", "Human"]
      2
      MostDoomSpace
      (Patrol MostDoomSpace (Just (LowestSkill Observation)))
      (2, 0)
      (-1, 0)
      (1, 0)
      []
      Strength
      Nothing
      "No one ever became muscle for the mob by asking too many questions."
  , monsterWith
      "morphic-terror"
      "Morphic Terror"
      1
      ["Formless Spawn"]
      0
      (NearestStreetTo (Just TheLeader))
      (Lurker (PlaceDoomAt AdjacentSpace (N 1)))
      (2, 1)
      (-1, -1)
      (2, 2)
      [Massive]
      Strength
      Nothing
      "Elite 1 (Has 1 additional health per investigator.) Massive (This monster engages and attacks each investigator in its space. It cannot be exhausted.)"
  , monsterWith
      "mouthy-raconteur"
      "Mouthy Raconteur"
      1
      ["Sheldon", "Human"]
      0
      MostDoomSpace
      (Lurker (PlaceDoomAt SourceSpace (N 1)))
      (1, 0)
      (0, -1)
      (1, 0)
      [Elusive]
      Strength
      Nothing
      "Elusive (This monster does not engage enemies in its space.) Some people just live to stir the pot."
  , monsterWith
      "rough-bootlegger"
      "Rough Bootlegger"
      2
      ["Sheldon", "Human"]
      2
      UnstableSpace
      (Patrol MostDoomSpace (Just (LowestSkill Strength)))
      (2, 0)
      (0, -1)
      (1, 0)
      []
      Strength
      Nothing
      "Whether running rum and guns, or just running off folks who poke their noses in where they do not belong, Sheldon's goons are happy to get their hands dirty."
  , monsterWith
      "siobhan-riley"
      "Siobhan Riley"
      1
      ["O'Bannion", "Servitor"]
      2
      (NearestStreetTo (Just TheLeader))
      (Hunter (HighestSkill Influence))
      (2, 1)
      (0, -2)
      (1, 1)
      []
      Strength
      Nothing
      "Elite 1 (Has 1 additional health per investigator.) After you defeat this monster as part of an attack action, you suffer one horror."
  , monsterWith
      "taloned-cannibal"
      "Taloned Cannibal"
      2
      ["Ghast"]
      2
      MostDoomSpace
      (Hunter (LowestSkill Influence))
      (2, 0)
      (-1, -1)
      (1, 1)
      []
      Strength
      Nothing
      "After this monster engages you, suffer one horror."
  , monsterWith
      "tunneling-dhole"
      "Tunneling Dhole"
      1
      ["Aberration"]
      0
      UnstableSpace
      (Lurker (ForInvestigators InSourceNeighborhood (SufferDamage (N 2))))
      (3, 0)
      (-1, -1)
      (2, 1)
      []
      Strength
      Nothing
      "After this monster attacks, it disengages all investigators and moves directly to the unstable space."
  , monsterWith
      "twilight-sentry"
      "Twilight Sentry"
      1
      ["Lodge", "Human"]
      2
      UnstableSpace
      (Lurker (ForInvestigators NearestToSource BecomeDelayed))
      (1, 0)
      (0, -2)
      (0, 1)
      [Elusive]
      Will
      Nothing
      "After you disengage this monster, suffer one horror."
  , monsterWith
      "undulating-mass"
      "Undulating Mass"
      1
      ["Formless Spawn"]
      2
      MostDoomSpace
      (Patrol MostDoomSpace (Just (LowestSkill Observation)))
      (2, 0)
      (-1, 0)
      (1, 2)
      []
      Strength
      Nothing
      "After this monster attacks, you discard one focus. Its oily body is wracked with constant spasms, growing and shedding limbs, organs, and faces."
  , monsterWith
      "screaming-haunt"
      "Screaming Haunt"
      1
      ["Troubled", "Spirit"]
      0
      UnstableSpace
      (Lurker (PlaceDoomAt SourceSpace (N 1)))
      (2, 0)
      (1, -2)
      (0, 2)
      [Elusive, Shrouded]
      Influence
      (Just "Haunting Dead")
      "After you engage this monster, place two doom in your space unless you become CURSED."
  , monsterWith
      "cacophonous-haunt"
      "Cacophonous Haunt"
      1
      ["Troubled", "Spirit"]
      0
      UnstableSpace
      (Lurker (PlaceDoomAt SourceSpace (N 1)))
      (2, 0)
      (0, -1)
      (0, 2)
      [Elusive, Shrouded]
      Will
      (Just "Haunting Dead")
      "After you engage this monster, you may place one doom in the unstable space to defeat this monster."
  , monsterWith
      "confounding-specter"
      "Confounding Specter"
      1
      ["Angry", "Spirit"]
      2
      MostDoomSpace
      (Patrol UnstableSpace (Just (LowestSkill Will)))
      (2, 0)
      (0, -1)
      (0, 1)
      [Shrouded]
      Will
      (Just "Raging Poltergeist")
      "After this monster attacks you, disengage all monsters and move directly to the unstable space. Then become delayed."
  , monsterWith
      "crashing-specter"
      "Crashing Specter"
      1
      ["Angry", "Spirit"]
      2
      MostDoomSpace
      (Patrol UnstableSpace (Just (LowestSkill Will)))
      (1, 1)
      (-1, 0)
      (1, 2)
      [Shrouded]
      Will
      (Just "Raging Poltergeist")
      "Elite 1 (This monster has one additional health per investigator.) After this monster attacks or becomes exhausted, it suffers one damage."
  , monsterWith
      "sanguinous-wraith"
      "Sanguinous Wraith"
      1
      ["Hostile", "Spirit"]
      2
      (NearestStreetTo (Just TheLeader))
      (Hunter (LowestSkill Lore))
      (2, 0)
      (-1, 0)
      (1, 1)
      [Shrouded]
      Lore
      (Just "Stalking Wraith")
      "After this monster attacks you, become FATIGUED. If you cannot, place one doom in your space."
  , monsterWith
      "vomitous-wraith"
      "Vomitous Wraith"
      1
      ["Hostile", "Spirit"]
      2
      (NearestStreetTo (Just TheLeader))
      (Hunter (LowestSkill Lore))
      (3, 0)
      (0, -1)
      (1, 2)
      [Shrouded, Retaliate]
      Will
      (Just "Stalking Wraith")
      "Retaliate (After you perform an attack action, if you did not damage this monster, it attacks you.)"
  ]
