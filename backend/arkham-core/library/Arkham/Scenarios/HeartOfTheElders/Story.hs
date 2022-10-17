module Arkham.Scenarios.HeartOfTheElders.Story where

import Arkham.Prelude

import Arkham.Message

intro1 :: FlavorText
intro1 = FlavorText
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
intro2 = FlavorText
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
intro3 = FlavorText
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
intro4 = FlavorText
  Nothing
  [ "The previous expedition’s journal doesn’t have all the answers, but as you\
    \ browse its pages, you are struck by a familiar diagram—one drawn by Lorenzo, an\
    \ expedition member who never made it back to Arkham. It is a delicate recreation\
    \ of a carving that was on the wall of one of the Eztli ruins. Many of the\
    \ patterns and hieroglyphs you see in this drawing are shared among these\
    \ pillars. It cannot simply be a coincidence. These pillars are surely the key to\
    \ finding your way to N’kai..."
  ]
