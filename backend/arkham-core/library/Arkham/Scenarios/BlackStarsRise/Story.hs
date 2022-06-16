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
