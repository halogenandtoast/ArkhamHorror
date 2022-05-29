module Arkham.Scenarios.ThePallidMask.Story where

import Arkham.Prelude

import Arkham.Message

intro1 :: Message
intro1 = FlavorText
  (Just "Intro 1")
  [ "You are stirred awake by dirty, stale air and dust in your throat. You are\
    \ lying on a slab of cold stone inside a lightless underground passageway.\
    \ How did you get here? And where is “here” exactly? A shiver courses up\
    \ your body, and your hair stands on end. Rising to your feet, you take\
    \ stock of your surroundings. Bones decorate the walls and ceiling around\
    \ you, open-mouthed skulls peering back at you everywhere you look."
  ]

intro2 :: Message
intro2 = FlavorText
  (Just "Intro 2")
  [ "The contents of Nigel Engram’s home answer none of your questions about The\
    \ King in Yellow, but do hint at where you should head next. The old, tattered\
    \ map you found on his coffee table depicts a section of the infamous\
    \ Catacombs of Paris. One particular room on the map has been circled with\
    \ pen, and next to it is written: “The key to opening the Path lies here!” You\
    \ swallow your fear and head immediately for the entrance to the catacombs\
    \ underneath Rue de la Tombe-Issoire."
  ]

harukosInformation :: Message
harukosInformation = FlavorText
  (Just "Haruko's Information")
  [ "Just past the archway closest to you, you see a familiar symbol etched into\
    \ the skull of a sheep: rows of concentric semicircles, lined with exotic\
    \ runes. Two wavy lines descend from the design, leading into the skull’s\
    \ lower jawbone. You recognize it as the pattern Haruko had shown you.\
    \ Wondering why it would appear here, you examine the skull in greater detail.\
    \ As soon as you touch the underside of the skull’s jawbone, its mouth\
    \ suddenly opens. Bones collapse to the ground as the wall slides to reveal a\
    \ new path."
  ]
