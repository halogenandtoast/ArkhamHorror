module Arkham.Scenarios.BlackStarsRise.Story where

import Arkham.Prelude

import Arkham.Message

intro :: Message
intro = FlavorText
  (Just "Scenario VII: Black Stars Rise")
  [ "The island commune of Mont Saint-Michel\
    \ lies off the northwestern coast of France. It is\
    \ beautiful, elegant, and enigmatic—a place out\
    \ of a fairy tale. Only this tale is one of horrors and\
    \ madness. During low tide, you would be able\
    \ to reach the island on foot by crossing the tidal\
    \ causeway that emerges from the sea. However,\
    \ by the time you reach the coast, the tide is much\
    \ higher than you’d anticipated. Dark clouds cover the sky, and a distant\
    \ crash of thunder signals the start of the oncoming storm. You find a\
    \ boat whose captain is willing to take you to the island, and prepare for\
    \ the ritual to come."
  ]

ashleighsInformation :: Message
ashleighsInformation = FlavorText
  (Just "Ashleigh's Information")
  [ "You recall the night where this all began,\
    \ and your thoughts drift to the mesmerizing song Ashleigh sang that\
    \ night. Somehow you remember the lyrics perfectly after all this time,\
    \ and its haunting melody is ingrained in your mind. “Above the city the\
    \ storm clouds rage, and waves crash through the gilded cage… Below\
    \ the earth the salt water seeps, the shadows fall as the red sun sleeps…”\
    \ Studying the island that lies before you, illuminated by flashes of\
    \ lighting and assaulted by tumultuous waves, you can’t help but wonder\
    \ if Ashleigh was singing about this very moment."
  ]

resolution1 :: Message
resolution1 = FlavorText
  (Just "Resolution 1")
  [ "You hold your breath as you swim through freezing water toward the spires\
    \ below. As you get closer, you see ripples throughout the water, as though you\
    \ are peering into a reflection upon the sea’s surface. You break through the\
    \ surface and gasp as air fills your lungs."
  ]

resolution2 :: Message
resolution2 = FlavorText
  (Just "Resolution 2")
  [ "You marvel at the cloud-waves encircling you and fly toward the spires above.\
    \ The desolate and inscrutable city of Carcosa towers before you. You suddenly\
    \ realize you are falling—not floating—and a pulling force takes hold of your\
    \ body."
  ]

resolution3 :: Message
resolution3 = FlavorText
  (Just "Resolution 3")
  [ "When you awaken, you are sitting in the choeur gothique of Mont Saint-Michel’s\
    \ abbey. No rain pelts the glass of the windows above, and you hear no thunder,\
    \ nor the screeching of winged horrors overhead. Somehow, you had escaped the\
    \ island’s certain doom. You creep to the door of the abbey and open it with\
    \ trepidation. A dazzling light sears your eyes, and you lift your hand to block\
    \ the glare of two suns. You stand overlooking the Lake of Hali. Black stars hang\
    \ in the heavens above. Beyond, over leagues of tossing cloud- waves, the towers\
    \ of Carcosa rise behind the shattered moon."
  ]
