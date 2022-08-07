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

noResolution :: Message
noResolution = FlavorText
  (Just "No Resolution")
  [ "You are shaken awake by a police officer and\
    \ lifted to your feet. You feel as though the weight of a train has\
    \ slammed into your head. The pain is unbearable. The man\
    \ shines a flashlight in your eyes and asks you several questions\
    \ in French. Dazed as you are, you find it difficult to answer. He\
    \ points toward the staircase nearby and pushes you away from\
    \ the catacombs. You stumble onto the Rue de la Tombe-Issoire\
    \ and slowly make your way to a hotel where you can stay the\
    \ night."
  , "Several days of research later, the meaning of the strange\
    \ diagrams you saw within the catacombs still escapes you. You\
    \ feel as though you have been led on a wild goose chase. Just as\
    \ you are about to give up, you see a faded yellow book on the\
    \ table nearest to you. You are stunned to discover that it is the\
    \ unabridged script of The King in Yellow. Drawn on the cover\
    \ in black ink is the very same diagram whose meaning you\
    \ have been struggling to interpret for several sleepless nights.\
    \ Who had placed it here? How had it come to your hotel room?\
    \ Regardless, you know what you must do. The play holds the\
    \ secrets—it has all along—and yet like a fool you have avoided\
    \ reading Act II out of superstition. Its words cannot harm you\
    \ any more than the creatures and fanatics you have already\
    \ encountered. Trembling, you open to the second part, and begin\
    \to read."
  ]

resolution1 :: Message
resolution1 = FlavorText
  (Just "Resolution 1")
  [ "The burned skull holds the key to everything.\
    \ You are sure of it. You have tried every method available to\
    \ you in studying the diagram it bears, but the answer eludes\
    \ you still. You have brought the skull to experts, occultists,\
    \ and professors. You have even tried speaking with the skull\
    \ on more than one occasion. Exasperated, you place the skull\
    \ on your night table and try to get some sleep for the first time\
    \ since escaping the catacombs. As you fall asleep, you cannot\
    \ shake the sight of the diagram etched in the skull’s forehead."
  , "You awaken with a spark of inspiration and rush to the\
    \ Musée du Louvre, a famous Parisian museum housing tens\
    \ of thousands of paintings, drawings, and archaeological\
    \ finds. You spend days exploring the museum—every display,\
    \ every collection, every single object of art that might hold a\
    \ clue to the diagram’s meaning. Finally, you see it: a painting\
    \ depicting a beautiful island town weathering a torrential\
    \ storm. Waves crash against the stone of the outer wall, the tide\
    \ threatening to swallow the island whole. Lightning flashes\
    \ around the tower of the abbey above the village. A whirlwind\
    \ of black clouds churns in the sky above. The diagram from\
    \ the burned skull you hold in your hands is recreated perfectly\
    \ in the stained glass of the abbey. The title of the piece is “The\
    \ Path Is Open.”"
  ]

resolution2 :: Message
resolution2 = FlavorText
  (Just "Resolution 2")
  [ "You fall ceaselessly through the empty abyss.\
    \ No air slows your descent or courses through your hair. It is\
    \ a passageway devoid of reality. Finally, you pass through an\
    \ invisible gateway and enter another realm. Looming above\
    \ and below you are two skylines, one a warped reflection of the\
    \ other. A vortex of swirling black clouds and crashing waves\
    \ lies in between them. You study both sides—the familiar city\
    \ and its strange mirror. Could this be the path to Carcosa? A\
    \ passageway between realities, where realms converge? If so, all\
    \ that is left is to find where this gate appears on Earth. You fall\
    \ into the vortex below."
  , "You are shaken awake by a police officer and lifted to your feet.\
    \ You feel as though the weight of a train has slammed into your\
    \ head. The pain is unbearable. The man shines a flashlight in\
    \ your eyes and asks you several questions in French. Your eyes\
    \ widen with realization and you wrest your arm free from the\
    \ confused man’s grip. “I have to go at once!”"
  ]
