module Arkham.Scenarios.BeyondTheGatesOfSleep.FlavorText where

import Arkham.Prelude

import Arkham.Text

guardianDream :: FlavorText
guardianDream =
  FlavorText
    (Just "Guardian ({guardian}) dream")
    [ "You circle a sullen hill shrouded in darkness and steeped in fog that seems to\
      \ swallow all sound. You don’t know what rests within the hill, but it feels\
      \ older than this world, and it seems like it is watching you. You dare not turn\
      \ to face the barrow, yet you know you cannot leave it, so instead you endlessly\
      \ patrol it. In the surrounding hedgerows, you catch glimpses of twisted shadows\
      \ darting to and fro, but whenever you turn your torch upon them, they vanish.\
      \ You begin to run but are frozen by a loud knock from behind you. When you turn,\
      \ the fog parts, and you finally see the base of the hill, where a heavy door of\
      \ weathered slate calls to you."
    ]

seekerDream :: FlavorText
seekerDream =
  FlavorText
    (Just "Seeker ({seeker}) dream")
    [ "You hurry through the halls of a university you barely recognize, cradling\
      \ several textbooks in your arms as you scurry past other students and\
      \ faculty members. You don’t know how it is possible that you forgot the date\
      \ of your exam, but if you don’t arrive soon, you know you will fail the\
      \ class. Somehow, you don’t even remember the course’s subject matter or its\
      \ curriculum, even though you know it is the end of the semester. It is not\
      \ like you to skip class or forget to study. You can hear the other students\
      \ snickering to themselves as you pass. When you reach the end of the hall,\
      \ the classroom door looms ominiously before you."
    ]

rogueDream :: FlavorText
rogueDream =
  FlavorText
    (Just "Rogue ({rogue}) dream")
    [ "You conceal a grin as you examine your cards. Jack of diamonds, queen of\
      \ spades for the nut straight. Your stack of chips is growing larger with\
      \ each hand, and this will be no different. Bets fly back and forth until it\
      \ is just you and the man in the white suit across from you. “Are you sure\
      \ about this?” he taunts, cradling his fingers. You push all of your chips\
      \ into the center and reveal your king-high straight. “What a shame.” He\
      \ tsks, revealing a pair of twos. “It appears you have lost everything.” You\
      \ protest, pointing to your superior hand, but when you examine it again, it\
      \ has changed. The figures on the face cards are now monstrous shapes with no\
      \ eyes, many mouths, and tentacled arms. “9♥-10♣-D♦-C♠-N{autoFail},” the cards read.\
      \ You gape in shock as you are carried away from the table and ushered out\
      \ the door."
    ]

mysticDream :: FlavorText
mysticDream =
  FlavorText
    (Just "Mystic ({mystic}) dream")
    [ "You traverse an enchanted path full of color and vibrancy. Wisps of\
      \ light hang in the air, bobbing up and down as you approach. With each step\
      \ you take, the flora along the path atrophy, following no laws of nature.\
      \ Flowers once bloomed in your wake; now they shrivel as you pass. Vines\
      \ wither and die. The wisps mock you for your ignorance, yet you do not slow\
      \ your pace. As you look upon the fair trees, their leaves begin to fall. The\
      \ once-green grass is dry, brown, and cracked. But even should the whole\
      \ forest die, it will be worth reaching the other side. When you do, the\
      \ trail ends abruptly at an ornate wooden door carved into the trunk of a\
      \ great redwood."
    ]

survivorDream :: FlavorText
survivorDream =
  FlavorText
    (Just "Survivor ({survivor}) dream")
    [ "You are fleeing through a dark, cramped hall of vine-covered wood.\
      \ Something inexplicable chases you through the murk. You are too terrified\
      \ to turn and look upon your pursuer, but nonetheless you know that if it\
      \ catches you, it will mean the end of your life. It will pierce your heart,\
      \ drain your blood, and devour your insides. It is this thought that drives\
      \ you onward, faster than you have ever run before. You cannot let it reach\
      \ you. You cannot let it feed. Suddenly, you spot your way out: a sturdy\
      \ wooden door surrounded by a wall of vines."
    ]

criminalDream :: FlavorText
criminalDream =
  FlavorText
    (Just "_Criminal_ dream")
    [ "The blare of sirens echoes through the streets behind you. They’re\
      \ gaining on you. They have always been hot on your heels, and you were\
      \ always one step ahead...until now. You run down an alleyway, and a\
      \ bright-red glare follows you. They’re going to catch you. They’ll lock you\
      \ in a cage and throw away the key. Your freedom, your lifestyle, everything\
      \ you are, everything you love: they’ll take it all away. And why? Just\
      \ because you wouldn’t play by their rules? When have they ever gotten\
      \ everything right? You round a corner just as the bulls are about to catch\
      \ up to you and spot the entrance into a brick building."
    ]

drifterDream :: FlavorText
drifterDream =
  FlavorText
    (Just "_Drifter_ dream")
    [ "You meander through a field of flowers, a warm spring breeze nudging\
      \ your steps forward. A train track divides the field, and as you meet its\
      \ parallel lines, the roaring clatter of steam and iron lumbers toward you.\
      \ As the train passes by, you grab onto a ladder hanging from one of its many\
      \ rusted and weather-beaten cars. Given the appearance of the exterior, the\
      \ train’s interior is full of unexpected luxury and affluence: Persian rugs\
      \ hang as decoration, marble staircases wind to dizzying heights, and a\
      \ crystal chandelier speckles the room with prismatic brilliance. Under the\
      \ chandelier sits an ornate doorway, its gilded lattices forming the\
      \ depiction of a fox in a forest."
    ]

hunterDream :: FlavorText
hunterDream =
  FlavorText
    (Just "_Hunter_ dream")
    [ "You stalk your prey through a decrepit house, its halls smelling of mold\
      \ and dust. The creature you hunt is an abomination from an unnatural world.\
      \ It threatens humanity’s very existence with its presence. You catch its\
      \ uncanny shape entering one of the doorways upstairs. It cannot escape\
      \ now—it is yours. But when you enter its den, you find only a dirty, cracked\
      \ mirror and your own tired and troubled face reflected in the glass. Where\
      \ could the aberration have gone? You are sure this is where it retreated to,\
      \ and yet...you turn back to the entrance and are surprised to see a door\
      \ where there was none before."
    ]

medicOrAssistantDream :: FlavorText
medicOrAssistantDream =
  FlavorText
    (Just "_Medic_ or _Assistant_ dream")
    [ "You stand in front of a closed casket. Mourners in black clothing stand\
      \ in a line at your side, waiting for you to say your last words. Tearfully,\
      \ you place a hand on the casket. The hard, cold wood feels dead under your\
      \ palm. They confided in you. They relied on you. And in their time of utmost\
      \ need, you failed them. All of this—the cold casket, the mourning friends\
      \ and family—it is all because of you. But when you open the casket, there is\
      \ no body inside: only a long stone passageway leading deep underground.\
      \ Suddenly, one of the mourners pushes you forward, and you fall over the\
      \ edge of the casket and down into the pit below, landing painfully on your\
      \ side. As you rise to your feet, you find your only exit: a stone archway\
      \ leading somewhere else entirely..."
    ]

miskatonicOrScholarDream :: FlavorText
miskatonicOrScholarDream =
  FlavorText
    (Just "_Miskatonic_ or _Scholar_ dream")
    [ "You are in an old, forgotten library, surrounded by the knowledge of the\
      \ ancients. Hundreds of thousands of tomes line the shelves around you,\
      \ ascending into an empty void above. The dim halls smell of musty pages and\
      \ melting wax. Taking one of the thick tomes from a nearby shelf, you begin\
      \ to read. Although you cannot seem to read any of the words, you are utterly\
      \ absorbed by the tale the pages weave. Your surroundings fade into\
      \ triviality as time passes. Nothing matters but the shapes of the crimson\
      \ ink; all else is meaningless in the face of such stark truths. All around\
      \ you, the library burns to its foundations. Just as the flames reach you, a\
      \ door out of the library calls to you. Somehow, it stands unburned among the\
      \ flames."
    ]

veteranDream :: FlavorText
veteranDream =
  FlavorText
    (Just "_Veteran_ dream")
    [ "You are in a muddy, soot-filled trench. All around you, the thunderous\
      \ din of war rages on and on, never ending. Dead litter the trenches: friends\
      \ and comrades-in-arms who lost their lives over nothing, in a land far from\
      \ home. You peer over the rim of the trench and into no-man’s-land, a barren\
      \ waste of broken, charred countryside where death is certain. And there, you\
      \ see it: a lone wooden door standing among the rubble and dirt. You know it\
      \ is your only way out. Gripping your rifle tight, you go over the top and\
      \ run headlong into a hail of deadly bullets, explosions rocking the ground\
      \ around you."
    ]

wayfarerDream :: FlavorText
wayfarerDream =
  FlavorText
    (Just "_Wayfarer_ dream")
    [ "You trudge chest-deep through a brackish swamp. Impossibly large\
      \ bulrushes tower over you, and clouds of strange, iridescent insects swarm\
      \ through the fetid air. With each step, your feet sink deeper into the soft\
      \ muck, threatening to pull you under. You feel something slick and cold\
      \ glide across your leg, and you lunge toward dry land—but the more you\
      \ struggle, the farther you sink. In the blink of an eye, you are consumed\
      \ whole by the mire, yet you are still falling ever downward. Eventually, you\
      \ are deposited onto a stone floor along with a small mudslide. You find\
      \ yourself in a sealed chamber, lit by the pale- blue glimmering of strange\
      \ hieroglyphs. The symbols frame an intricate golden door: the door to the\
      \ crypt you’ve been searching for."
    ]

neutralDream1 :: FlavorText
neutralDream1 =
  FlavorText
    (Just "Neutral dream")
    [ "You toss and turn but do not sleep, not even for a moment. Your mind is\
      \ filled with discomfort and dark thoughts: thoughts of failure, of\
      \ ineptitude, of loss. Your brow is covered in sweat. No matter how you lie,\
      \ your bed is either too hot or too cold. Finally, sick of getting nowhere,\
      \ you rise and head to the sink to splash some water on your face. That is\
      \ when you realize the layout of your room is not as it was before—and the\
      \ door to your bathroom has been replaced with a large gateway of onyx and\
      \ marble."
    ]

neutralDream2 :: FlavorText
neutralDream2 =
  FlavorText
    (Just "Neutral dream")
    [ "You sit on the back patio of what seems like your childhood home, but\
      \ something is off. You don’t recognize your family...the plants in the back\
      \ yard are arranged differently...and the sky is a patchwork of rotting\
      \ corpses, raining dismembered body parts onto the landscape. Your not-family\
      \ watches this precipitation of corpses calmly, discussing the weather it as\
      \ if it were a banal occurrence. This continues even as the intermittent\
      \ thudding of hands and feet onto your roof intensifies into a torrent of\
      \ maimed torsos that causes the ceiling to sag. You run outside, weaving\
      \ through the deadly rain, and head instinctively toward the edge of your\
      \ parents’ property. As you hear the crash of the house collapsing behind\
      \ you, you push away the fallen limbs piling atop an old, familiar spot and\
      \ are relieved to find the weathered wood of a cellar door."
    ]
