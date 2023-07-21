module Arkham.Scenarios.TurnBackTime.Story where

import Arkham.Prelude

import Arkham.Message

intro1 :: FlavorText
intro1 =
  FlavorText
    (Just "Intro 1")
    [ "You stand in front of the main temple of the Eztli, sweat pouring down your\
      \ skin due to the hot, damp jungle air. Stunned, you examine your surroundings.\
      \ José and Maria stand next to you, gaping in wonder at the temple’s\
      \ sophisticated architecture. Their packs are bulging with the extra supplies\
      \ they were able to acquire from Alejandro’s contacts in Mexico City. Alejandro\
      \ himself stands at the rear of the group, grinning."
    , "Your consciousness has been snapped so far back that your mind aches from the\
      \ force of it. Your body shudders, and you struggle to resist collapsing out of\
      \ exhaustion. Though you have altered fate, your true task still remains ahead of\
      \ you. As the rest of the expedition sets up camp and prepares to explore the\
      \ ruins, you sneak off to fulfill your purpose, leaving Alejandro and the rest of\
      \ your group behind. They wouldn’t understand. They haven’t seen what you have\
      \ seen."
    ]

intro2 :: FlavorText
intro2 =
  FlavorText
    (Just "Intro 2")
    [ "You stand near the main temple of the Eztli, sweat pouring down your skin due\
      \ to the hot, damp jungle air. Stunned, you examine your surroundings. Several\
      \ tents are set up in the clearing in front of the temple. José stands guard\
      \ along the border of your camp, and you can see Maria packing equipment and\
      \ provisions into her backpack, getting ready for her journey to the northern\
      \ edge of the jungle, as you had instructed. Alejandro sits on a log in the\
      \ center of the camp, decrypting the inscription he found near the temple’s\
      \ entryway."
    , "Your consciousness has been snapped so far back that your mind aches from the\
      \ force of it. Your body shudders, and you struggle to resist collapsing out of\
      \ exhaustion. Though you have altered fate, your true task still remains ahead of\
      \ you. As the rest of the expedition prepares to explore the ruins, you sneak off\
      \ to fulfill your purpose, leaving Alejandro and the rest of your group behind.\
      \ They wouldn’t understand. They haven’t seen what you have seen."
    ]

noResolution :: FlavorText
noResolution =
  FlavorText
    (Just "No Resolution")
    [ "With your failure, the paradox of clashing timelines catches up to you. Despite\
      \ all you have done, the events of the future cannot be unmade. You will always\
      \ find the relic. You will always fail to preserve it. And the Nexus will always\
      \ falter."
    , "Reality as you know it will never be the same, but perhaps this is not the end\
      \ of the world. After all, time is just one dimension: a dimension humanity\
      \ barely understands. We experience time, but we cannot see it. We can measure\
      \ it, but we cannot alter it. So what if the fourth dimension has split into\
      \ hundreds of thousands of planes of existence? Our feeble human minds cannot\
      \ comprehend the outcome, but the universe still survives, its ever-expanding\
      \ boundaries no longer limited by the confines of time."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "Thursday, December 17th, 1925"
    , "No one else has ever learned of the events that truly occurred during the\
      \ summer of 1925, and no one ever will, for I intend to keep this journal locked\
      \ in my safe-deposit box once I have finished writing in it. I seek only to\
      \ preserve my own record of these events, for I do not wish the passage of time\
      \ to erase them completely. After all, these are not my memories, but the\
      \ memories of my other self: the self who ventured through time and space to save\
      \ humanity."
    , "It is difficult to explain how we did what we did. After all that\
      \ happened—Ichtaca, Alejandro, the city of the Yithians, Yoth—it turns out the\
      \ true doom of humanity had occurred much, much earlier: when we first left to\
      \ find the Eztli. We brought all of these events upon ourselves. In order to\
      \ prevent this calamity, we had to seal the relic away forever. I remember these\
      \ events as though I myself had experienced them, though for me, the expedition\
      \ had gone much differently. We reached the central chamber of the Eztli ruins,\
      \ but it was sealed shut, and nothing—not even the TNT that José had brought from\
      \ the trucks—could breach the entrance. Although we returned to Arkham with proof\
      \ that the Eztli existed, no other expedition would ever find them or their ruins\
      \ again. My other self perished in those ruins—but I remember. Somehow, I\
      \ remember everything."
    ]
