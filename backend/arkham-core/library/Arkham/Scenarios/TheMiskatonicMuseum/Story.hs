module Arkham.Scenarios.TheMiskatonicMuseum.Story
where

import Arkham.Prelude

import Arkham.Text

intro1 :: FlavorText
intro1 = FlavorText
  (Just "Scenario II: The Miskatonic Museum")
  [ "Several months ago, Armitage and his colleagues\
    \ stopped a rampaging horror from tearing\
    \ through Dunwich, a backwater town several\
    \ hours north and west of Arkham. At first you\
    \ imagine this beast as a rabid bear, or worse, but\
    \ the professor’s description of the creature paints\
    \ a different picture."
  , "It all began when a man named Wilbur Whateley entered the Orne\
    \ Library looking for Olaus Wormius’s Latin translation of a book called\
    \ the Necronomicon. Wilbur already possessed a beaten-up English\
    \ translation by Dr. John Dee, but it was insufficient for his purposes.\
    \   Armitage turned the man away, fearing what use the strange man had\
    \ for the book. Whateley returned in secret, hoping to steal the book ,\
    \ but was attacked by a hound guarding the university. Armitage, Rice,\
    \ and Morgan later discovered Whateley’s body. A description of the foul\
    \ corpse—semi-anthropomorphic and covered in fur, with a leathery\
    \ hide and greenish-grey tentacles—causes you to question whether or\
    \ not Whateley was truly human."
  ]

intro2 :: Bool -> FlavorText
intro2 True = FlavorText
  Nothing
  [ "The notes written by Dr. Armitage in the journal stress\
    \ Whateley’s desire to get his hands on the Necronomicon for some\
    \ terrible purpose. As you read on, it seems that Dr. Armitage brought\
    \ the university’s copy of the tome to Harold Walsted—the curator of\
    \ the Miskatonic Museum—for safekeeping in the museum’s Restricted\
    \ Hall. Although you are worried about your mentor, you are equally\
    \ worried that Armitage’s kidnappers might get their hands on this\
    \ Necronomicon. You decide to head to the museum to prevent them\
    \ from acquiring it."
  ]
intro2 False = FlavorText
  Nothing
  [ "“My colleagues and I were quick to put the ordeal behind\
    \ us,” Armitage says with a sigh. “But it seems that things haven’t\
    \ fully resolved themselves. I’ll tell you the rest later, but for now, it is\
    \ imperative that we get our hands on that copy of the Necronomicon.\
    \ If my instincts are correct, the assailants you’ve encountered will be\
    \ searching for it. After all that transpired, I didn’t feel safe keeping it\
    \ at the library, so I brought it to my good friend, Harold Walsted. He is\
    \ the current curator of the Miskatonic Museum. I thought it would be\
    \ safe in the museum’s Restricted Hall, but now I’m not so sure. You must\
    \ retrieve it at all costs! I fear terribly what they could do with the rites\
    \ contained in its pages…”"
  ]

noResolution :: FlavorText
noResolution = FlavorText
  Nothing
  [ "Whatever the creature in the\
    \ museum was, you had neither the will nor the tools to destroy\
    \ it. It seems you must give up any hope of recovering the\
    \ Necronomicon. Even so, there are others depending on you.\
    \ Gathering your courage, you prepare for your next task."
  ]

resolution1 :: FlavorText
resolution1 = FlavorText
  (Just "Resolution 1")
  [ "As long as this translation of the\
    \ Necronomicon exists, there will be sorcerers and other foul\
    \ agents like Whateley seeking it. In the end, you know what\
    \ must be done to protect humanity from the threats you’ve seen.\
    \ You find a trash bin and fill it with books and documents,\
    \ throwing the Necronomicon on top. It takes several matches\
    \ to set the contents of the bin alight. The flames fill the room\
    \ with heat and the creeping shadows retreat from its light. You\
    \ watch the book burn for some time, its pages turning to ash.\
    \ You can only hope you’ve made the right decision."
  ]

resolution2 :: FlavorText
resolution2 = FlavorText
  (Just "Resolution 2")
  [ "The Necronomicon is more than just a book;\
    \ it is a tool. Within its pages is a wealth of information about\
    \ the forces and creatures you have encountered. Knowing how\
    \ useful it could be in your endeavors, how could you possibly\
    \ bring yourself to destroy it? Besides, as long as you keep the\
    \ book safely in your possession, you will still be foiling those\
    \ who wish to use it for nefarious purposes."
  ]
