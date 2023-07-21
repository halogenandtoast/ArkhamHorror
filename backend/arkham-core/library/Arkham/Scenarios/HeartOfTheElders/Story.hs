module Arkham.Scenarios.HeartOfTheElders.Story where

import Arkham.Prelude

import Arkham.Message

intro1 :: FlavorText
intro1 =
  FlavorText
    (Just "Scenario V-A: Heart of the Elders, Part I")
    [ "It has been several days since your party ventured forth into the rainforest\
      \ once more, and each day, the hazards you face grow more dangerous and frequent.\
      \ Securing enough food and supplies for the journey is a constant struggle, and\
      \ you are harrassed at every turn by the jungle’s inhabitants. Fortunately, you\
      \ have seen no sign yet of the serpent people who attacked the previous\
      \ expedition, but you’re positive they are out there. Watching. Waiting."
    , "On the morning of the sixth day, you find the entrance. It is a huge cave\
      \ opening, partially obscured by trees, brush, and vines. You nearly missed the\
      \ great cavern maw altogether, despite its enormity. Surrounding the cavern’s\
      \ entrance are six stone pillars, each covered in strange grooves and carvings\
      \ that glow with a dim red hue. Perhaps one of your guides will understand what\
      \ this means?"
    ]

intro2 :: FlavorText
intro2 =
  FlavorText
    Nothing
    [ "“The entrance is warded against intruders,” Ichtaca says cryptically, her eyes\
      \ closed as though probing the cavern with unnatural senses. “These pillars are\
      \ the key. Each is a test of wit and will.” She steps toward one of them and\
      \ touches it with her palm, and its glow brightens. You notice now that the\
      \ grooves along the pillar’s foundation depict a pattern you have seen somewhere\
      \ before. If Ichtaca believes this place to be warded, you trust her\
      \ opinion—there must be some way to break these wards..."
    ]

intro3 :: FlavorText
intro3 =
  FlavorText
    Nothing
    [ "Alejandro examines each of the pillars studiously. “It is a lock of some sort.\
      \ Look here, see? The grooves do not line up perfectly.” He twists one of the\
      \ pillars, and you can see the pattern along its side shifting. “I wager if we\
      \ entered the cave, we would not get very far without these pillars in the\
      \ correct formation. Just a hunch, I suppose...But I think we should examine this\
      \ area further before we proceed.” You know Alejandro well enough by now to know\
      \ that his hunches tend to be correct."
    ]

intro4 :: FlavorText
intro4 =
  FlavorText
    Nothing
    [ "The previous expedition’s journal doesn’t have all the answers, but as you\
      \ browse its pages, you are struck by a familiar diagram—one drawn by Lorenzo, an\
      \ expedition member who never made it back to Arkham. It is a delicate recreation\
      \ of a carving that was on the wall of one of the Eztli ruins. Many of the\
      \ patterns and hieroglyphs you see in this drawing are shared among these\
      \ pillars. It cannot simply be a coincidence. These pillars are surely the key to\
      \ finding your way to N’kai..."
    ]

introB :: FlavorText
introB =
  FlavorText
    (Just "Scenario V–B: Heart of the Elders, Part 2")
    [ "You descend down miles of steep, cold stone. The tunnels are chokingly narrow\
      \ at first, filled with rubble and detritus. Finally, you reach level ground, and\
      \ the cavern’s true nature reveals itself before you. It is as though there were\
      \ an entire world below the surface; the cavern’s ceiling is like a stone curtain\
      \ of sky above you. Even more vast and terrifying are the endless depths below\
      \ you, beyond the edges of stone chasms and archaic bridges. On all sides, this\
      \ underground realm seems to extend beyond the boundary of your sight; you wonder\
      \ just how far it reaches. Perhaps the entirety of the known surface of the Earth\
      \ is but the top layer of another world..."
    ]

noResolutionA :: FlavorText
noResolutionA =
  FlavorText
    (Just "No Resolution")
    [ "The hazards of the jungle are too dangerous for you to continue exploring. If\
      \ you remain any longer, these odd pillars will mark your grave. You decide to\
      \ flee for now, returning to a familiar clearing several miles away where you can\
      \ recuperate. You know that you must eventually return to the pillars and figure\
      \ out the pattern that will open the way forward...but for now, at least you are\
      \ safe."
    ]

resolution1A :: FlavorText
resolution1A =
  FlavorText
    (Just "Resolution 1")
    ["Swallowing your fear, you enter the cavern..."]

noResolutionB :: FlavorText
noResolutionB =
  FlavorText
    (Just "No Resolution")
    [ "You awaken, lying on rough stone in a place much colder and deeper than before.\
      \ You stand and take stock of your surroundings, noting the red glow that seems\
      \ to emanate from the cavern to your left. You follow it for several minutes,\
      \ until finally it leads you into a well-lit entryway, filled with stone pillars.\
      \ At the far end of the room lies an ornate and elaborately decorated doorway, a\
      \ stone disc that has been rolled aside to unveil the hellish realm beyond."
    ]

resolution1B :: FlavorText
resolution1B =
  FlavorText
    (Just "Resolution 1")
    [ "“Finally! Yoth, cavern of the serpents! The cursed ones, who fled the\
      \ destruction of Valusia!” Alejandro examines the entryway with wonder in his\
      \ eyes, arms raised in exultation. You know not of what he speaks, and you are\
      \ beginning to grow worried. Just then, something scutters behind you, and a\
      \ sound like the click-clacking of a claw catches you by surprise. Emerging from\
      \ the darkness behind you are several alien creatures, the likes of which you\
      \ have never seen: ten feet tall, with conical bodies that end in four strange\
      \ appendages of varying lengths. “There you are,” Alejandro says calmly. One of\
      \ the creatures responds by clicking its claws against one another in a\
      \ deliberate pattern. “Take their minds,” Alejandro commands the creatures. “We\
      \ have no further use for them.” It is your last human memory."
    ]

resolution2B :: FlavorText
resolution2B =
  FlavorText
    (Just "Resolution 2")
    [ "Before you are able to cross into the next layer of the cavern, a sound like\
      \ the click-clacking of a claw catches you by surprise. Emerging from the\
      \ darkness behind you are several alien creatures, the likes of which you have\
      \ never seen: ten feet tall, with conical bodies that end in four strange\
      \ appendages of varying lengths. You are stunned to see a human figure standing\
      \ at the head of the group: none other than Alejandro Vela. “Take their minds,”\
      \ Alejandro commands the creatures. “We have no further use for them.” It is your\
      \ last human memory."
    ]
