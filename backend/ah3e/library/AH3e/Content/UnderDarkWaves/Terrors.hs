{- | Terror decks. A terror card is attached to a neighborhood's encounter deck
and read before the encounter there; which of its three sections you read is
decided by the terror tokens on that neighborhood, not by doom.
-}
module AH3e.Content.UnderDarkWaves.Terrors (cards) where

import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill

cards :: [CardDef]
cards = fromBox UnderDarkWaves (feedingFrenzy <> frozenCity)

pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

terror :: Text -> Text -> Int -> [((Int, Maybe Int), Text, Effect)] -> CardDef
terror slug name n sections =
  CardDef
    (CardCode (slug <> "-" <> pad n))
    name
    CoreSet
    1
    (TerrorCard (TerrorDef name [(range, Encounter txt eff) | (range, txt, eff) <- sections]))

{- | "Then discard two terror from your neighborhood", which closes nearly every
deepest band.
-}
calm :: Effect
calm = Custom "discard-two-terror"

doomHere :: Int -> Effect
doomHere n = PlaceDoomAt YourSpace (N n)

-- | "<this> unless <that>": the first is what happens, the second buys it off.
orElse :: Text -> Effect -> Text -> Effect -> Effect
orElse lblA a lblB b = Choose [(lblA, a), (lblB, b)]

discardAlly :: Effect
discardAlly = Pay (CostDiscard AllyCard) NoEffect

spawnNonHuman, spawnNonHumanHere :: Effect
spawnNonHuman = Custom "spawn-non-human"
spawnNonHumanHere = Custom "spawn-non-human-here"

feedingFrenzy :: [CardDef]
feedingFrenzy =
  [ ff
      1
      [
        ( (0, Just 2)
        , "The squat, fish-like man waddles toward you, a frothy drool trickling from the edges of his wide mouth. Become CURSED when he sprays that mucus across your face unless you discard one remnant to distract him with a gobbet of strange meat."
        , orElse "Become CURSED" cursed "Discard one remnant" (Pay (SpendRemnants 1) NoEffect)
        )
      ,
        ( (3, Just 4)
        , "With a wet spatter, the creature before you sloughs off what remains of its human skin, revealing the still-soft scales of its new form. You may become delayed to defeat the beast now, before it fully awakes. If you do not, spawn one non-human monster."
        , MayPay CostDelayed NoEffect spawnNonHuman
        )
      ,
        ( (5, Nothing)
        , "A Deep One sorcerer threatens to set his cohort free to feast unless you offer yourself to him. Move one doom from your space to the scenario sheet unless you offer to serve him and gain a DARK PACT. Whether you gain the condition or not, discard two terror from your neighborhood."
        , Seq
            [ orElse
                "Move one doom to the scenario sheet"
                (Seq [RemoveDoomFrom YourSpace (N 1), DoomOnSheet (N 1)])
                "Gain a DARK PACT"
                (Pay (CostCondition "DARK PACT") NoEffect)
            , calm
            ]
        )
      ]
  , ff
      2
      [
        ( (0, Just 1)
        , "There are so many of the scaly, spined creatures blocking the road ahead of you. Ducking behind a corner, you wait for the pack of Deep Ones to move on (will). If you pass, they depart after a few tense moments. If you fail, you quail at the thought of those creatures finding you; suffer one horror."
        , Test Will 0 NoEffect (horror 1)
        )
      ,
        ( (2, Just 3)
        , "You spot a creature on the prowl, and realize that you have a chance to lead it away from a populated area (observation -1). If you pass, you leave a false trail far away from anyone this beast could harm. If you fail, your ruse is unsuccessful; spawn one monster."
        , Test Observation (-1) NoEffect SpawnMonster
        )
      ,
        ( (4, Nothing)
        , "A young couple is out on the street, cut off by a pack of ravenous Deep Ones. When they run to you for aid, you try to find a safe route for them to escape (observation -2). If you fail, they don't make it; place two doom in your space. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Observation (-2) NoEffect (doomHere 2), calm]
        )
      ]
  , ff
      3
      [
        ( (0, Just 2)
        , "You can hear heavy, rasping breath from the darkness and look furtively about for the creature responsible (observation). If you pass, you spot and avoid the Deep One. If you fail, the beast pounces, spattering your face with black phlegm; become TAINTED."
        , Test Observation 0 NoEffect tainted
        )
      ,
        ( (3, Just 3)
        , "The Deep Ones that prowl the street call to each other with a warbling, watery song (will -1). If you fail, the strange melody makes your head swirl with visions of shedding your soft skin and crawling into the sea; suffer two horror."
        , Test Will (-1) NoEffect (horror 2)
        )
      ,
        ( (4, Nothing)
        , "You spot a figure prowling the dark streets, searching for prey. You may become delayed to lure it into a trap and get the drop on it. If you do not, it keeps hunting; spawn one non-human monster. Whether you do or not, discard two terror from your neighborhood."
        , Seq [MayPay CostDelayed NoEffect spawnNonHuman, calm]
        )
      ]
  , ff
      4
      [
        ( (0, Just 2)
        , "The beasts that prowl these streets have huge, crooked teeth set in their slavering maws (will). If you pass, you resolve to stop these creatures. If you fail, you are preoccupied with thoughts of those teeth tearing into your soft flesh; suffer one horror."
        , Test Will 0 NoEffect (horror 1)
        )
      ,
        ( (3, Just 4)
        , "You brace yourself against the wooden door, as whatever is following you bashes against the other side (strength -1). If you fail, the onslaught knocks you away from the door and sends you sprawling; spawn one non-human monster in your space."
        , Test Strength (-1) NoEffect spawnNonHumanHere
        )
      ,
        ( (5, Nothing)
        , "Something is there, lurking in the briny mist. You tighten your grip on your lamp and search for the beast (observation -2). If you fail, you cannot prevent it from finding its prey; place two doom in your space. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Observation (-2) NoEffect (doomHere 2), calm]
        )
      ]
  , ff
      5
      [
        ( (0, Just 1)
        , "With a screech, the Deep One hybrid pins you to a wall and tries to clamp its round, lamprey-like mouth onto your neck (strength). If you pass, you shove it away and escape. If you fail, the ring of needle-like teeth leaves behind a black, weeping lesion; become TAINTED."
        , Test Strength 0 NoEffect tainted
        )
      ,
        ( (2, Just 3)
        , "The ragged beast lifts its head and sniffs the air before letting loose a pitiful lowing cry. You watch it from your hiding place and realize that it is calling to its friends (will -1). If you pass, you interrupt it. If you fail, you freeze in place; spawn one monster."
        , Test Will (-1) NoEffect SpawnMonster
        )
      ,
        ( (4, Nothing)
        , "The slavering beast tries to get past you to hunt easier prey (strength -2). If you pass, you bar its path while its quarry escapes. If you fail, the creature swats you away; place two doom in your space. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Strength (-2) NoEffect (doomHere 2), calm]
        )
      ]
  , ff
      6
      [
        ( (0, Just 2)
        , "You find a good vantage point, and try to identify any new threats (observation). If you pass, you note where the strongest beasts lurk and find a route past them. If you fail, you don't know how you'll stop the creatures all around you; spawn one monster."
        , Test Observation 0 NoEffect SpawnMonster
        )
      ,
        ( (3, Just 3)
        , "An acrid stink fills the air, burning your throat and making your vision blur (observation -1). If you pass, you see a clear path out of here. If you fail, you linger too long, and the foul substance fills your lungs; become TAINTED."
        , Test Observation (-1) NoEffect tainted
        )
      ,
        ( (4, Nothing)
        , "The scream is cut short by a howl from some unseen creature and a wet crunch (will -2). If you pass, you swear to avenge the unknown victim. If you fail, the grim sound will not leave your mind; suffer two horror. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Will (-2) NoEffect (horror 2), calm]
        )
      ]
  , ff
      7
      [
        ( (0, Just 2)
        , "A smoky brazier spits and pops as the reagents burn in offering, beckoning some foul creature (strength). If you pass, you topple the brazier and scatter the ashes. If you fail, the pungent smoke calls to a new threat; spawn one monster."
        , Test Strength 0 NoEffect SpawnMonster
        )
      ,
        ( (3, Just 4)
        , "The howling, rattling bay of so many creatures on the hunt sets your nerves on edge (will -1). If you pass, you resolve to thin the predators' numbers. If you fail, you are convinced that the beasts are all around you; suffer two horror."
        , Test Will (-1) NoEffect (horror 2)
        )
      ,
        ( (5, Nothing)
        , "The sheer number of creatures that lurk here have filled the air with a putrid miasma (will -2). If you pass, you scatter the swarm. If you fail, the foul pollution calls more of the beasts; spawn one non-human monster. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Will (-2) NoEffect spawnNonHuman, calm]
        )
      ]
  , ff
      8
      [
        ( (0, Just 1)
        , "The incessant howling of the thronging horde fills your mind with thoughts of the sea (will). If you pass, you shake your head clear with renewed purpose. If you fail, the visions never truly leave you; become TAINTED."
        , Test Will 0 NoEffect tainted
        )
      ,
        ( (2, Just 3)
        , "A spiny creature stoops over the road ahead, catching the scent of some unknown prey. You try to get close enough to get the drop on the beast (observation -1). If you fail, it hears your approach and flees beyond your reach; spawn one monster."
        , Test Observation (-1) NoEffect SpawnMonster
        )
      ,
        ( (4, Nothing)
        , "You escape a scuffle with a hunting Deep One with only minor scrapes, but the scratches it left on your arm start to itch (will -2). If you fail, wriggling worms protrude from your graying flesh; become CURSED. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Will (-2) NoEffect cursed, calm]
        )
      ]
  , ff
      9
      [
        ( (0, Just 2)
        , "With a raspy howl, a hunched Deep One courses after you on its bowed legs (strength). If you pass, you easily outpace the creature and escape. If you fail, you only narrowly elude it before you collapse to regain your panicked breath; suffer one horror."
        , Test Strength 0 NoEffect (horror 1)
        )
      ,
        ( (3, Just 3)
        , "You help a group of civilians erect a hasty barricade to keep out the swarming Deep Ones (strength -1). If you pass, the defenses hold securely. If you fail, the barrier shatters to splinters under the clawing hands of the horde; place one doom in your space."
        , Test Strength (-1) NoEffect (doomHere 1)
        )
      ,
        ( (4, Nothing)
        , "A stooped figure trails the broken manacles that once confined it (strength -2). If you pass, you seize the rusted chain and trap it once more. If you fail, the chain is wrenched from your grip; spawn one non-human monster. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Strength (-2) NoEffect spawnNonHuman, calm]
        )
      ]
  , ff
      10
      [
        ( (0, Just 2)
        , "You lock eyes with the too-human gaze of a ragged, spined figure. You feel your mind brush against its own while it beckons its allies to find you (will). If you pass, you conceal your presence. If you fail, spawn one monster."
        , Test Will 0 NoEffect SpawnMonster
        )
      ,
        ( (3, Just 3)
        , "The prowling Deep One wears the tatters of a naval officer's uniform. He demands that you offer him a tribute, and you find it impossible to refuse his commands. Become TAINTED unless you spend one remnant to sate him."
        , orElse "Become TAINTED" tainted "Spend one remnant" (Pay (SpendRemnants 1) NoEffect)
        )
      ,
        ( (4, Nothing)
        , "The surging throng of Deep Ones is guided by some malevolent will. You may become CURSED to draw the dark presence into yourself to contain it. If you do not, draw and resolve one mythos token. Whether you do or not, discard two terror from your neighborhood."
        , Seq [MayPay (CostCondition "CURSED") NoEffect (DrawMythosTokens 1), calm]
        )
      ]
  , ff
      11
      [
        ( (0, Just 2)
        , "The briny mist makes it hard to see anything, but you can hear talons and spines clicking against the hard ground (observation). If you pass, you see a shrouded form slipping away. If you fail, you feel certain that something is about to pounce; suffer one horror."
        , Test Observation 0 NoEffect (horror 1)
        )
      ,
        ( (3, Just 4)
        , "A pale-eyed man croaks out a dark litany as he seeks to summon a host of ravenous creatures (strength -1). If you pass, you overpower him before he can complete his prayer. If you fail, he completes his ritual; spawn one non-human monster."
        , Test Strength (-1) NoEffect spawnNonHuman
        )
      ,
        ( (5, Nothing)
        , "A swarm of blank-faced men with tell-tale bulbous eyes and wide mouths surges toward you (strength -2). If you fail, the group overwhelms you and their corruption seeps into you; become TAINTED. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Strength (-2) NoEffect tainted, calm]
        )
      ]
  , ff
      12
      [
        ( (0, Just 1)
        , "Deep Ones surge through the area, chanting the names of their twisted divinities (lore). If you pass, you block the influence they seek to spread here. If you fail, each name lands like a cut on your soul; place one doom in your space."
        , Test Lore 0 NoEffect (doomHere 1)
        )
      ,
        ( (2, Just 3)
        , "Many harsh, inhuman voices rasp threateningly in the gathered, salty gloom (will -1). If you pass, you calmly withdraw and find a sanctuary. If you fail, you know that you are surrounded by dangerous and unseen foes; suffer one horror."
        , Test Will (-1) NoEffect (horror 1)
        )
      ,
        ( (4, Nothing)
        , "A distant howl draws your attention to the horizon, but you feel a closer threat (observation -2). If you pass, you elude a lurking beast. If you fail, spawn one non-human monster in your space. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Observation (-2) NoEffect spawnNonHumanHere, calm]
        )
      ]
  ]
 where
  ff = terror "feeding-frenzy" "Feeding Frenzy"

frozenCity :: [CardDef]
frozenCity =
  [ fc
      1
      [
        ( (0, Just 1)
        , "A deluge of sleet and ice cascades all around you, rattling to the ground and forcing you to withdraw to safety. Not everyone you are with makes it to your shelter unscathed. Suffer one damage and one horror; you must assign this damage and horror to an ally, if possible."
        , Custom "terror-ally-takes-the-harm"
        )
      ,
        ( (2, Just 4)
        , "You hear a chanting voice carried on the wind, seeking to summon greater evil to this place (lore -1). If you pass, you counter the stranger's efforts. If you fail, the dark storm intensifies; place one doom in your space."
        , Test Lore (-1) NoEffect (doomHere 1)
        )
      ,
        ( (5, Nothing)
        , "Ice and snow threaten to choke the life out of this place. You walk the frozen streets alone, and you begin to fear that there is nothing you can do to bring any amount of warmth to the world. Place two doom in your space. Then discard two terror from your neighborhood."
        , Seq [doomHere 2, calm]
        )
      ]
  , fc
      2
      [
        ( (0, Just 1)
        , "As you warm yourself near the fire, a voice whispering at the edge of your hearing instructs you to extinguish the flame (will). If you pass, you stoke the flames and shut out the insidious voice. If you fail, you stamp out the fire; place one doom in your space."
        , Test Will 0 NoEffect (doomHere 1)
        )
      ,
        ( (2, Just 3)
        , "You wipe frozen tears from your eyes and press on through the driving storm. The biting wind blisters your face, and it feels as though you'll never be warm again. Suffer one damage."
        , damage 1
        )
      ,
        ( (4, Nothing)
        , "The voices of the dead echo on the wind, beseeching you to bring them more friends (will -2). If you fail, you isolate yourself out of the fear that you'll harm the people around you; place one doom in your space. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Will (-2) NoEffect (doomHere 1), calm]
        )
      ]
  , fc
      3
      [
        ( (0, Just 2)
        , "You think you remember an old Celtic charm that can hold back the grasp of winter (lore). If you pass, you quiet the winds long enough to press on. If you fail, your efforts do nothing to ease the storm; place one doom in your space."
        , Test Lore 0 NoEffect (doomHere 1)
        )
      ,
        ( (3, Just 4)
        , "The bus from Innsmouth is stuck in a snowbank, and Joe Sargent is having trouble keeping the handful of passengers safe and calm. Place one doom in your space unless you become delayed to wait out the storm with them."
        , orElse "Place one doom in your space" (doomHere 1) "Become delayed" (Pay CostDelayed NoEffect)
        )
      ,
        ( (5, Nothing)
        , "Forging ahead through the raging storm, you lose track of some of your comrades. You circle back to look for the people you've lost, but you are unable to find everyone. Discard one ally; if you cannot, place one doom in your space. Then discard two terror from your neighborhood."
        , Seq [Custom "terror-discard-ally-or-doom", calm]
        )
      ]
  , fc
      4
      [
        ( (0, Just 2)
        , "The blowing snow and icy ground makes it quite hazardous to travel anywhere until the wind dies down. Suffer one damage unless you become delayed to wait for a break in the weather."
        , orElse "Suffer one damage" (damage 1) "Become delayed" (Pay CostDelayed NoEffect)
        )
      ,
        ( (3, Just 3)
        , "The chill creeping into your limbs is far from natural, and you can feel a dark mind at work in the growing storm (lore -1). If you fail, that darkness seeps into you until you can no longer feel the cold; become CURSED."
        , Test Lore (-1) NoEffect cursed
        )
      ,
        ( (4, Nothing)
        , "A voice on the wind calls to you. As it caresses every syllable of your name, you begin to feel feverishly warm, craving the icy touch of winter (will -2). If you fail, you run out into the storm; suffer two damage. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Will (-2) NoEffect (damage 2), calm]
        )
      ]
  , fc
      5
      [
        ( (0, Just 2)
        , "The dark night around you is silent save for the quiet hiss of the snowfall, and you cannot see another soul. It is almost peaceful, until you hear the mournful cry of some inhuman creature from the darkness. Place one doom in your space."
        , doomHere 1
        )
      ,
        ( (3, Just 5)
        , "The presence lurking in the storm begs for someone to keep it company for a while, and it becomes clear that it won't let anyone leave unless a willing companion joins it. Stay with the spirit and become TAINTED unless you discard an ally."
        , orElse "Become TAINTED" tainted "Discard an ally" discardAlly
        )
      ,
        ( (6, Nothing)
        , "You would swear that you can see anguished, screaming faces reflected in the creeping ice that surrounds you. Even when you leave the frozen hollow behind, you can still see the screaming faces. Become CURSED. Then discard two terror from your neighborhood."
        , Seq [cursed, calm]
        )
      ]
  , fc
      6
      [
        ( (0, Just 1)
        , "You are traveling with a group when you get separated in the storm. Lost and disoriented, you hear one of your companions calling for help (will). If you fail, you struggle to find them in the whiteout; an ally suffers two damage."
        , Test Will 0 NoEffect (Custom "terror-ally-suffers-two")
        )
      ,
        ( (2, Just 3)
        , "Someone has left a length of rope knotted around an ice-encrusted tree (lore -1). If you pass, you realize the pattern of knots is a coded message directing you to shelter from the cold. If you fail, you wander aimlessly through the snow; suffer two damage."
        , Test Lore (-1) NoEffect (damage 2)
        )
      ,
        ( (4, Nothing)
        , "Caught in a raging storm of blown ice and snow, the only shelter you can find still leaves you terribly exposed. However you arrange yourself, one person will be caught in the ice. Suffer two direct damage unless you discard an ally. Regardless, discard two terror from your neighborhood."
        , Seq [orElse "Suffer two direct damage" (DirectDamage (N 2)) "Discard an ally" discardAlly, calm]
        )
      ]
  , fc
      7
      [
        ( (0, Just 2)
        , "The facade of the building in front of you is encased in ice. Heavy but fragile, the ice will need to be cleared before you try to enter (lore). If you fail, you knock the ice down upon yourself; suffer one damage from the sharp, heavy chunks."
        , Test Lore 0 NoEffect (damage 1)
        )
      ,
        ( (3, Just 3)
        , "You see warm light from the shelter just up ahead, but your limbs grow heavy as you plod through the drifting snow (will -1). If you pass, you struggle through it. If you fail, you collapse a short distant from the door; suffer one damage."
        , Test Will (-1) NoEffect (damage 1)
        )
      ,
        ( (4, Nothing)
        , "The snow drifts swirl into unnatural shapes and form a massive glyph ahead of you (lore -2). If you fail, you blunder into the middle of the symbol and are soon awash in darkness; become CURSED. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Lore (-2) NoEffect cursed, calm]
        )
      ]
  , fc
      8
      [
        ( (0, Just 2)
        , "Your bones ache and you can see nothing but the white field of driving snow. A voice beyond your perception offers you respite from the cold, if only you'll stop and rest for a moment (will). If you fail, you heed the tempting voice; become CURSED."
        , Test Will 0 NoEffect cursed
        )
      ,
        ( (3, Just 4)
        , "The frozen world around you shines and sparkles in the low sun like a fractured mirror. Snow blind, you stagger and fall, sliding down a slope of jagged ice that tears at your skin and clothing. You suffer two damage and struggle to find your feet."
        , damage 2
        )
      ,
        ( (5, Nothing)
        , "The blizzard rages even stronger than before, and you are forced to find shelter. You could press on, but you are certain that it will be hazardous. Become delayed unless you suffer three damage. Whether you do or not, discard two terror from your neighborhood."
        , Seq [orElse "Become delayed" (Pay CostDelayed NoEffect) "Suffer three damage" (damage 3), calm]
        )
      ]
  , fc
      9
      [
        ( (0, Just 2)
        , "A shiver runs up your spine, and the cold air and drifting snow are not wholly responsible. You can tell that there is something out in the storm, hunting, although you can neither see it nor stop it. Place one doom in your space."
        , doomHere 1
        )
      ,
        ( (3, Just 3)
        , "The creeping cold and biting wind are dangerous, but you endure it, dressed warmly against the unnatural elements. Others have not prepared as you have, and you can tell the cold is taking its toll. An ally in your space suffers two damage."
        , Custom "terror-ally-in-space-suffers-two"
        )
      ,
        ( (4, Nothing)
        , "Someone has carved a baleful glyph into the razor-sharp spire of ice that blocks your way (lore -2). If you pass, you safely deface the ward. If you fail, the ice shatters violently and riddles you with jagged shards; suffer two damage. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Lore (-2) NoEffect (damage 2), calm]
        )
      ]
  , fc
      10
      [
        ( (0, Just 1)
        , "The desolate, frozen streets echo with a crushing emptiness (will). If you pass, you soldier on, confident that you'll find other people in the storm. If you fail, you despondently linger too long in the cold; suffer one damage."
        , Test Will 0 NoEffect (damage 1)
        )
      ,
        ( (2, Just 4)
        , "A dying man writhes in the snow, mad with an unnatural hunger. You know the presence inside him will seek a new host when he passes on, but he begs you not to let him die alone. Become CURSED unless you become delayed and TAINTED."
        , orElse
            "Become CURSED"
            cursed
            "Become delayed and TAINTED"
            (Pay (AllOf [CostDelayed, CostCondition "TAINTED"]) NoEffect)
        )
      ,
        ( (5, Nothing)
        , "A tempting whisper grows into a roaring gale. The voice on the icy wind demands a host. If you do not give it what it wants, it will take what it needs from your friends. Become TAINTED unless you discard an ally. Whether you do or not, discard two terror from your neighborhood."
        , Seq [orElse "Become TAINTED" tainted "Discard an ally" discardAlly, calm]
        )
      ]
  , fc
      11
      [
        ( (0, Just 1)
        , "The heavy, swirling snow is making it impossible for your meager fire to provide any useful heat (lore). If you pass, you bolster the flame with a simple charm you've learned. If you fail, the biting cold makes you lose feeling in your extremities; suffer one damage."
        , Test Lore 0 NoEffect (damage 1)
        )
      ,
        ( (2, Just 3)
        , "The woman huddled against the wind pleads for your help, her face streaked with tears and someone else's blood (will -1). If you pass you calmly but warily realize that she is a victim too. If you fail, you turn and flee from the cannibal; place one doom in your space."
        , Test Will (-1) NoEffect (doomHere 1)
        )
      ,
        ( (4, Nothing)
        , "You find two bodies entombed in ice, fear etched across their faces and hands clasped in solidarity. Place one doom in your space unless you become delayed to chisel the couple free of their icy prison. Whether you do or not, discard two terror from your neighborhood."
        , Seq
            [ orElse "Place one doom in your space" (doomHere 1) "Become delayed" (Pay CostDelayed NoEffect)
            , calm
            ]
        )
      ]
  , fc
      12
      [
        ( (0, Just 1)
        , "The frigid weather has reduced the road before you to a plane of sheer black rime punctuated by jagged spires of razor-sharp ice. You'll need to go slowly to pick your way carefully forward, lest you injure yourself. Become delayed unless you suffer one damage."
        , orElse "Become delayed" (Pay CostDelayed NoEffect) "Suffer one damage" (damage 1)
        )
      ,
        ( (2, Just 3)
        , "The persistent, howling wind begins to call your name. As you try to shut it out, you feel an angry presence pressing against you, threatening to bring death to others if you refuse to breathe it in. Place one doom in your space unless you become TAINTED."
        , orElse
            "Place one doom in your space"
            (doomHere 1)
            "Become TAINTED"
            (Pay (CostCondition "TAINTED") NoEffect)
        )
      ,
        ( (4, Nothing)
        , "The smell of fresh meat and blood overwhelms you when you stumble across the remains of some hoary beast (will -2). If you fail, the hot blood stains your face and hands when you feed; become CURSED. Whether you pass or fail, discard two terror from your neighborhood."
        , Seq [Test Will (-2) NoEffect cursed, calm]
        )
      ]
  ]
 where
  fc = terror "frozen-city" "Frozen City"
