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

resolution1 :: Message
resolution1 = FlavorText
  (Just "Resolution 1")
  [ "You stand atop the balcony of the abbey tower in Mont Saint-Michel, wet wind\
    \ coursing through your hair. The Pallid Mask lies in your hands, devoid of\
    \ warmth. You can scarcely bring yourself to look at it. You remember everything\
    \ now—how it all began, how it ended, and everything in between. There is only\
    \ one thing left to do. You hurl the mask into the air with all of your strength\
    \ and watch as it flies into the tempest-tossed waves below."
  ]

resolution2 :: Message
resolution2 = FlavorText
  (Just "Resolution 2")
  [ "A roar of applause startles you awake. The crowd of the theatre rises to their\
    \ feet, cheering endlessly. Several patrons in the front row are throwing roses\
    \ to the actors on stage, who are bowing with wide smiles across their faces. A\
    \ searing headache crawls through your temples. How did you get back here? Were\
    \ you in the theatre all along? What about the dinner party? The asylum? The\
    \ catacombs?... Carcosa? Was it all just a terrible dream?"
  , "You exit the auditorium before the crowd rushes to do the same. Dizziness and\
    \ exhaustion harries your senses. The weight of your lengthy investigation is\
    \ crushing your mind. By the time you finally reach the lobby, you are clawing\
    \ your way across the ground.  Just then, a man in an elegant suit reaches down\
    \ and grabs you by your arm, pulling you to your feet. You are about to thank him\
    \ when you realize who he is. The sight of him causes you to stagger backward in\
    \ horror momentarily. The actor who plays the role of the Stranger is still\
    \ wearing his pale, featureless mask. He gives you a wordless bow, then turns and\
    \ leaves through the Ward Theatre’s front entrance."
  ]

resolution3 :: Message
resolution3 = FlavorText
  (Just "Resolution 3")
  [ "When you awaken, you are sitting in the choeur gothique of Mont Saint-Michel’s\
    \ abbey. No rain pelts the glass of the windows above, and you hear no thunder,\
    \ nor the screeching of winged horrors overhead. Somehow, you have escaped. You\
    \ creep to the door of the abbey and open it with trepidation. A dazzling light\
    \ sears your eyes, and you lift your hand to block the glare of the sun. You\
    \ stand overlooking beautiful Mont Saint-Michel. Seagulls fly over the abbey\
    \ under a backdrop of cloudless sky. The waters around the abbey are calm and\
    \ azure-blue."
  , "“I’ve been looking all over for you!” You hear a voice exclaim behind you. You\
    \ turn with a startle. For a moment you expect to see the Stranger and his Pallid\
    \ Mask, but instead, the boat captain you’d hired to take you to Mont\
    \ Saint-Michel stands at the edge of the steps, smiling at you. “Are you ready to\
    \ head back?”"
  ]

resolution4 :: Message
resolution4 = FlavorText
  (Just "Resolution 4")
  [ "When you awaken, you are sitting in the choeur gothique of Mont Saint-Michel’s\
    \ abbey. No rain pelts the glass of the windows above, and you hear no thunder,\
    \ nor the screeching of winged horrors overhead. Somehow, you have escaped the\
    \ island’s certain doom. You creep to the door of the abbey and open it with\
    \ trepidation. A dazzling light sears your eyes, and you lift your hand to block\
    \ the glare of two suns. You stand overlooking the Lake of Hali. Black stars hang\
    \ in the heavens above. Beyond, over leagues of tossing cloud-waves, the towers\
    \ of Carcosa rise behind the shattered moon."
  ]

resolution5 :: Message
resolution5 = FlavorText
  (Just "Resolution 5")
  [ "The outside world will never know how you tried to save them. They treat you\
    \ with pity, with scorn and contempt, but you know the truth. They are resigned\
    \ to their hideous fate, but still you try day after day to bring them your\
    \ message. You must warn them of The King in Yellow, of Hastur and of Carcosa.\
    \ “Another lost soul,” they say, but they cannot possibly fathom what you have\
    \ gone through in your attempt to save them. You warn them that he is coming to\
    \ claim them, but they never believe you. You write of Aldebaran and of the black\
    \ stars on the wall of your cell, but the words become faded as the years go by.\
    \ Your warning is never heeded."
  ]
