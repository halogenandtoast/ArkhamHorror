{- | Mystery encounter decks. A mystery card reads its top effect, which names two
courses of action in bold, and then one of the two sections below it; the secondary
effects are not read before the choice is made (Under Dark Waves, p. 8).
-}
module AH3e.Content.SecretsOfTheOrder.Mysteries (cards) where

import AH3e.Content.Tiles
import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import Data.Text qualified as T

cards :: [CardDef]
cards = fromBox SecretsOfTheOrder (theWitchHouse <> theUnnamable)

card :: Text -> Int -> (Text, Effect) -> [(Text, Text, Effect)] -> CardDef
card place n (intro, introEffect) branches =
  CardDef
    { code = CardCode (coerce sid <> "-" <> T.justifyRight 2 '0' (tshow n))
    , name = place <> " " <> tshow n <> "/8"
    , expansion = CoreSet
    , copies = 1
    , kind =
        MysteryCard
          MysteryDef
            { space = sid
            , opening = Encounter intro introEffect
            , branches = [(header, Encounter txt eff) | (header, txt, eff) <- branches]
            }
    }
 where
  sid = spaceIdFor place

-- | "Remove one doom from any space", which only offers the spaces holding any.
anywhere :: Int -> Effect
anywhere n = RemoveDoomFrom AnySpace (N n)

theWitchHouse :: [CardDef]
theWitchHouse =
  [ card
      "The Witch House"
      1
      ( "A pile of tattered clothing draws your attention to a vacant room under one of the gables. Gain one remnant. As you study the abandoned objects, you are startled by a rat with a strangely human countenance. You may chase after it or study the mess it left behind."
      , remnants 1
      )
      [
        ( "Chase After It"
        , "You pursue the creature down a cracked and broken hallway (observation). If you pass, you find the creature skulking in a small alcove, where you also uncover the journal of a long-dead occultist; gain one spell. If you fail, the last you see of the wretched creature are its too-human eyes; suffer two horror."
        , Test Observation 0 spell (horror 2)
        )
      ,
        ( "Study the Mess"
        , "Ignoring the horrific pest, you turn your gaze to the creature's leavings. Amongst the tattered remains you find traces of dozens of people that this creature lured to their deaths (will). If you pass, you resolve to avenge these lost souls; become DRIVEN. If you fail, the evil that was done here overwhelms you; become CURSED."
        , Test Will 0 driven cursed
        )
      ]
  , card
      "The Witch House"
      2
      ( "The misery in this place fills you with purpose. Become DRIVEN. At the top of the attic stairs, a rat with matted fur turns its grimacing human-like face toward you. It crooks a finger to beckon you before it disappears into a dark room. You may follow it or seal the door behind the grotesque creature."
      , driven
      )
      [
        ( "Follow It"
        , "You follow the familiar until it vanishes through an odd corner. The angles of the room make the edge of your vision squirm, and a disembodied voice offers you knowledge. Reveal the top three spells in the deck. You may gain a DARK PACT to gain one of them. Place any spells you do not gain on the bottom of the deck."
        , Custom "witch-house-spells"
        )
      ,
        ( "Seal the Door"
        , "That creature is a harbinger, and no good will come from following it. You slam the door behind it and do your best to hold it fast against the presence that presses from the other side (strength). If you pass, the door holds; remove one doom from any space and you become BLESSED. If you fail, a shadow bursts through; suffer two horror."
        , Test Strength 0 (Seq [anywhere 1, blessed]) (horror 2)
        )
      ]
  , card
      "The Witch House"
      3
      ( "You hear a pitiful sobbing from behind the wall in the cellar. As you approach, a large patch of plaster sloughs off the wall, exposing lath boards covered in a lattice-work of arcane symbology; gain one remnant. You may search for a door into the neighboring room, or break through the wall."
      , remnants 1
      )
      [
        ( "Search for a Door"
        , "There must be a way through the wall; surely, no one would build a room with no door (observation). If you pass, a discolored patch of plaster covers a long-abandoned doorway. If you fail, you are certain someone was walled up and left to die; suffer two horror."
        , Test Observation 0 NoEffect (horror 2)
        )
      ,
        ( "Break Through"
        , "You throw your shoulder against the crumbling plaster (strength). If you pass, you fall through the wall and find yourself in an attic bedroom, surrounded by arcane text that dances through the muted sunlight; gain one spell. If you fail, you crash into a solid wooden beam; suffer two damage."
        , Test Strength 0 spell (damage 2)
        )
      ]
  , card
      "The Witch House"
      4
      ( "The liquid spilled on the floor is pooling in the wrong places. You can see obvious dips worn into the old floorboards, but the water gathers elsewhere. You resolve to decipher this riddle. Become DRIVEN. You may survey the room or prise up the floorboards under the unnatural puddle."
      , driven
      )
      [
        ( "Survey the Room"
        , "You methodically pace out the room to determine its dimensions, but you get different results with every attempt (lore). If you pass, you find a series of runes carved into the woodwork, distorting your perception of the space; gain one remnant and spawn one clue. If you fail, you try again and again to take an accurate measurement; become delayed."
        , Test Lore 0 (Seq [remnants 1, SpawnOneClue]) delayed
        )
      ,
        ( "Prise up the Floorboards"
        , "You pull up the floorboards, desperate to find the cause of this phenomenon. An animal's skeleton coils around a bundle of filthy cloth (will). If you pass, you unwrap the bundle and find a notched, black-handled knife; gain the BLACKENED ATHAME. If you fail, the skull stares right through you; become CURSED."
        , Test Will 0 (named "BLACKENED ATHAME") cursed
        )
      ]
  , card
      "The Witch House"
      5
      ( "You find a moss-covered chess pawn in the yard. Gain one remnant. Up above, you spot a figure silhouetted in a round window. She sees you and begins to beat her fists against the glass, calling mutely for you. You may watch her or try to find the room with the round window."
      , remnants 1
      )
      [
        ( "Watch Her"
        , "Once she has your attention, she begins to signal you with esoteric gestures. You watch her carefully (lore). If you pass, you find that the movements hold arcane significance; gain one spell. If you fail, you watch uncomprehendingly for too long, and the growing shadow behind her suddenly snuffs her out; suffer two horror."
        , Test Lore 0 spell (horror 2)
        )
      ,
        ( "Find the Room"
        , "You sprint through the twisting stairways and tight corridors of the old boarding house attempting to find the room with the round window (observation). If you pass, you find the correct room, and the woman gives you a reassuring smile before she vanishes; become BLESSED. If you fail, you take a wrong turn; become FATIGUED."
        , Test Observation 0 blessed fatigued
        )
      ]
  , card
      "The Witch House"
      6
      ( "A few rooms here still house desperate lodgers. You hear a raving voice from behind one such door, muttering arcane secrets. Gain one spell. The occupant explodes out of the door, accusing you of stealing his thoughts. You may reason with the crazed tenant or restrain him."
      , spell
      )
      [
        ( "Reason"
        , "You try to talk him down, calmly explaining that you didn't mean to listen in on him. He shoves you against the wall and you repeat yourself, staring into his bloodshot eyes (influence). If you pass, he tells you where he learned the words you heard him say; remove one doom from any space. If you fail, he smashes you into the wall and flees; suffer one damage."
        , Test Influence 0 (anywhere 1) (damage 1)
        )
      ,
        ( "Restrain"
        , "Fearing the man poses a danger to you and himself, you do your best to immobilize him (strength). If you pass, you get him under control and he tells you how to find an odd statue in his rooms; gain one remnant. If you fail, he strains against you before he runs off into the streets howling curses that chill you to your core; suffer two horror."
        , Test Strength 0 (remnants 1) (horror 2)
        )
      ]
  , card
      "The Witch House"
      7
      ( "A traveler from Salem took a room here, but by the look of things, he's been missing for some time. His personal effects are strewn across the floor of his abandoned room, along with a few scattered ritual candles. Gain one remnant. You may search the room carefully or question a neighbor."
      , remnants 1
      )
      [
        ( "Search"
        , "You close the door behind you and quietly examine everything in the abandoned room (observation). If you pass, you find his journal and confirm that he was an investigator of the occult; gain one spell as you study his notes. If you fail, you are sure you are missing something, and search the room from top to bottom without result; become delayed."
        , Test Observation 0 spell delayed
        )
      ,
        ( "Question"
        , "The woman next door eyes you suspiciously and refuses to open her door more than a sliver (influence). If you pass, she warms to you, and explains over a cup of tea that he departed abruptly for Boston after he met with a tall, thin stranger; become DRIVEN. If you fail, her pupils grow black and she slams the door in your face; suffer two horror."
        , Test Influence 0 driven (horror 2)
        )
      ]
  , card
      "The Witch House"
      8
      ( "In the dingy second-floor washroom, you find an old journal secreted away in a pigeonhole behind the mirror. Scanning through it, you find arcane diagrams scribbled in the margins. Gain one spell. You have limited time; you may read the first entry or the last entry in the journal."
      , spell
      )
      [
        ( "First Entry"
        , "In the first entry, the young woman describes meeting a handsome stranger shortly after arriving in town. She says looking into his eyes was like seeing a field of stars (observation). If you pass, you recognize her description of their meeting place; become DRIVEN and spawn one clue. If you fail, a chill runs down your spine; suffer two horror."
        , Test Observation 0 (Seq [driven, SpawnOneClue]) (horror 2)
        )
      ,
        ( "Last Entry"
        , "The woman describes her efforts to repel some kind of supernatural force that was following her (lore). If you pass, you find her notes detailing the ritual she planned to perform; gain a spell and one remnant. If you fail, you swear you see the thing she describes reflected in the cracked washroom mirror; suffer two horror."
        , Test Lore 0 (Seq [spell, remnants 1]) (horror 2)
        )
      ]
  ]

theUnnamable :: [CardDef]
theUnnamable =
  [ card
      "The Unnamable"
      1
      ( "A bulbous, deformed rat scampers into a gap in the baseboard and you hear the tell-tale skittering of hundreds of claws. A concerned stranger looks to you anxiously, eager to be away from here; gain one ally. You may try your best to ignore the scratching or you may attempt to drive the rats out."
      , ally
      )
      [
        ( "Ignore the Scratching"
        , "You step into the parlor and try to search the room, despite the incessant skittering (will). If you pass, you keep a cool head and locate something hidden in a small heap of tattered refuse on a long-cold hearth; gain one curio. If you fail, the sounds get louder and louder until a wave of rats bursts through one of the walls; suffer two horror."
        , Test Will 0 curioItem (horror 2)
        )
      ,
        ( "Drive the Rats Out"
        , "You grab a fire poker and bang as hard as you can on the walls, hoping to scare the beasts toward the door (strength). If you pass, the rats scatter into the night; remove one doom from any space. If you fail, the poker smashes a hole in the wall and dozens of rats wash over you, biting at your legs; suffer two damage."
        , Test Strength 0 (anywhere 1) (damage 2)
        )
      ]
  , card
      "The Unnamable"
      2
      ( "You see a stumbling, expressionless stranger absently drop a useful item; gain one curio. Eyes glazed over with a murky yellow light, they mindlessly scratch unknown symbols into the plaster walls of the parlor. You may attempt to free them from the possession or sneak past them."
      , curioItem
      )
      [
        ( "Free Them"
        , "You cannot bring yourself to ignore a soul in need. Test lore to dispel the spirit or pay one remnant to tempt it to leave its victim. If you pass or spend the remnant, the stranger is grateful to be free; gain one ally. If you fail, the stranger lashes out in a sudden rage at your approach; suffer two damage."
        , Choose
            [ ("Test lore", Test Lore 0 ally (damage 2))
            , ("Spend one remnant", Pay (SpendRemnants 1) ally)
            ]
        )
      ,
        ( "Sneak Past"
        , "You have nothing to offer this wayward soul, so you make your way around them (observation). If you pass, you slip past and manage to grab something away from them as you do; gain one curio. If you fail, the glassy-eyed stranger howls in an unholy rage and summons something terrible; spawn one non-human monster."
        , Test Observation 0 curioItem (Custom "spawn-inhuman-monster")
        )
      ]
  , card
      "The Unnamable"
      3
      ( "Small bones are scattered across the floor. Gain one remnant. The sweeping light of your lantern settles on a vanity, draped with a dingy canvas. You hear a scraping sound coming from under the cloth, like a rusty nail carving across a frozen lake. You may uncover the vanity or smash it to the ground."
      , remnants 1
      )
      [
        ( "Uncover the Vanity"
        , "You pull the heavy, dusty cloth to the floor and reveal a warped mirror. Your reflection doesn't look right... (observation). If you pass, you realize your reflection moves with a mind of its own, reaching through the glass to give you something; gain one curio. If you fail, the malformed reflection places its finger to its lips and disappears."
        , pass Observation 0 curioItem
        )
      ,
        ( "Smash It"
        , "The noise is maddening, and you grab at the vanity (strength). If you pass, you topple it to the ground in a shattering crash; gain one curio as something slides across the ground from under the broken wreck. If you fail, you struggle to move the dressing table, and a clawed hand lunges from beneath the cover; suffer one damage."
        , Test Strength 0 curioItem (damage 1)
        )
      ]
  , card
      "The Unnamable"
      4
      ( "Scattered objects lie forgotten in a drafty room. Gain one curio. A mottled-leather journal seems to be writing itself in a cramped and irregular hand. The words are confusing, but you might be able to decipher them. You may study the past entries or watch the present scrawling."
      , curioItem
      )
      [
        ( "Study the Past"
        , "You gently turn the journal back several pages (lore). If you pass, you realize the author of the haphazardly-written journal has somehow predicted several future calamities; remove one doom from any space. If you fail, the words make little sense, but your own name is repeated several times; suffer one horror."
        , Test Lore 0 (anywhere 1) (horror 1)
        )
      ,
        ( "Watch the Present"
        , "You move closer and study the invisible force's rapid scrawling. You recognize your own name etched onto the pages, and that the author seems to be trying to warn you about something, but you need to study the words carefully to make sense of it. You may become delayed to become BLESSED."
        , mayPay CostDelayed blessed
        )
      ]
  , card
      "The Unnamable"
      5
      ( "A bloody, discarded satchel sets you on edge. Gain one remnant. As you inch down the hallway, a nearby doorknob begins to rattle violently, as someone or something attempts to get through it. You may open the door to investigate or barricade it against whatever lurks there."
      , remnants 1
      )
      [
        ( "Open the Door"
        , "You wrench open the door and a woman tumbles out, breathing heavily. AQUINNAH joins you. \"We... have to run!\" she pants (observation). If you pass, you help her up and the two of you escape into the night; you may move one space. If you fail, something lurches forward out of the dark room; spawn one monster in your space."
        , Seq [named "AQUINNAH", Test Observation 0 (MoveUpTo 1) (SpawnMonsterIn YourSpace False)]
        )
      ,
        ( "Barricade It"
        , "You push a bookcase in front of the door. Remove one doom from any space. A moment later, something slams hard against it, straining to reach you (strength). If you pass, you hold the door until all grows still. If you fail, the door splinters around you before all goes black and you come to in the garden; become CURSED."
        , Seq [anywhere 1, Test Strength 0 NoEffect cursed]
        )
      ]
  , card
      "The Unnamable"
      6
      ( "The books in this old library are mostly unreadable. Gain one remnant. An ornate tapestry catches your eye, and the pattern shifts and swirls as you stare at it. You reach toward it, and the pattern ripples away from your hand, like oil on a pond. You may touch the tapestry or study the pattern."
      , remnants 1
      )
      [
        ( "Touch the Tapestry"
        , "You brush your trembling fingers across the fluid images (will). If you pass, you open your mind to unexplainable things; move to any space and remove one doom there. If you fail, you feel every inch of Arkham in your head at the same moment, each raindrop and piece of cobbled stone; become FATIGUED."
        , Test Will 0 (Seq [MoveDirectlyTo AnySpace, RemoveDoomFrom YourSpace (N 1)]) fatigued
        )
      ,
        ( "Study the Pattern"
        , "You hold down your shaking hand and resolve to study the strange pattern (observation). If you pass, you see a glimpse of the future; remove one doom from any space. If you fail, the patterns reveal something you cannot comprehend; suffer one horror."
        , Test Observation 0 (anywhere 1) (horror 1)
        )
      ]
  , card
      "The Unnamable"
      7
      ( "You step into another room and your stomach knots. Every surface in this place is covered in spirals of writing that sharpen your mind to a razor edge. Focus one skill of your choice. Looking at the twisting, breathing words makes your skin prickle. You may study the text or deface it."
      , focusAny
      )
      [
        ( "Study the Text"
        , "The words come alive as you study them, and they whisper to you of a promise of power\8212for a price. You may gain a DARK PACT to gain two spells. If you refuse the offer, the crescendo of fell voices begin screaming your name in a horrifying, agonizing unison; suffer one horror."
        , MayPay (CostCondition "DARK PACT") (Seq [spell, spell]) (horror 1)
        )
      ,
        ( "Deface It"
        , "You resolve to destroy all physical trace of this evil (strength). If you pass, you shatter every object and surface you can until you find a momentary peace; remove one doom from any space. If you fail, the twisting spirals close in on you, whispering dark secrets; suffer one horror."
        , Test Strength 0 (anywhere 1) (horror 1)
        )
      ]
  , card
      "The Unnamable"
      8
      ( "In a wood-paneled study, a friendly stranger puzzles over a stately wardrobe that has been sealed with a thick lock. Gain one ally. Something about the wardrobe compels you to open it. You may attempt to pick the lock or smash the wardrobe open."
      , ally
      )
      [
        ( "Pick the Lock"
        , "You produce a bobby pin and get to work picking the lock, sweat forming on your forehead (observation). If you pass, you finally get it open, but only find a small trinket; gain one curio. If you fail, your sweating intensifies and you soon realize you have been working for hours; become FATIGUED."
        , Test Observation 0 curioItem fatigued
        )
      ,
        ( "Smash the Wardrobe"
        , "The wardrobe begs to be opened, and you know that you must see what lies within (strength). If you pass, you laugh manically as you smash open the doors, and find yourself devastated at the simple thing you find; gain one curio. If you fail, you bang on the doors with your fists until they bleed; suffer one damage."
        , Test Strength 0 curioItem (damage 1)
        )
      ]
  ]
