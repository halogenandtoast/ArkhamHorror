module Arkham.Scenarios.DimCarcosa.Story where

import Arkham.Prelude

import Arkham.Message

intro1 :: Message
intro1 = FlavorText
  (Just "Intro 1")
  [ "You swim to the edge of the water and throw yourself upon the rocky shore,\
    \ gasping for air. You lie there for some time, utterly exhausted. Every muscle\
    \ in your body aches. Before you lies a desolate expanse of plain. In the\
    \ distance, the alien spires of a warped city rise into the clouds. Behind you,\
    \ the dark surface of the lake from which you’d emerged reflects the glare of two\
    \ suns. When you peer back into the murky depths, you see no sign of the chapel\
    \ where you’d made the leap, or of Mont Saint-Michel, or even of Earth. This is\
    \ Carcosa—the realm of madness in the stars, where Hastur reigns supreme."
  ]

intro2 :: Message
intro2 = FlavorText
  (Just "Intro 2")
  [ "You land on a piece of black obsidian stone winding up a dark spire. You cough\
    \ blood and feel a searing pain in your gut, as though you’d broken a rib. Before\
    \ you lies a warped, alien city. Its twisting streets and aberrant architecture\
    \ have no semblance of order or structure. In the distance, a murky lake reflects\
    \ the glare of two suns. When you look up, the abbey of Mont Saint-Michel peeks\
    \ just below the clouds, flipped upside-down. This is Carcosa—the realm of\
    \ madness in the stars, where Hastur reigns supreme."
  ]
