module Arkham.Scenarios.EchoesOfThePast.Story where

import Arkham.Prelude

import Arkham.Types.Message

intro :: Message
intro = FlavorText
  (Just "Scenario III: Echoes of the Past")
  [ "Your head throbs with a dull ache as you drive\
    \ through the rainy streets of Arkham toward your\
    \ next destination. The threat of the Stranger looms\
    \ in your mind, and you find yourself glancing often\
    \ at your rear-view mirror, expecting to see the\
    \ expressionless visage of his mask haunting you.\
    \ Instead, you see nothing but the misty, starless\
    \ night, and the deserted road behind you."
  , "Your thoughts once again wander, as they have often in the past few hours,\
    \ to The King in Yellow and to the city of Carcosa and its inhabitants.\
    \ What was the message hidden inside that awful play, the meaning within\
    \ its madness? A lone detail worms its way to the forefront of your thoughts,\
    \ one made apparent by the discussions you’d overheard at Ms. Dumaine’s\
    \ estate—that tonight’s performance of The King in Yellow was not the\
    \ first Arkham had seen of the foul play. There had been at least one other\
    \ performance, directed by the same man: Nigel Engram."
  , "There is one place in Arkham where records are often kept of important\
    \ events occuring within the city: the Historical Society’s manor house in\
    \ Southside. If there are any records of the previous show of The King in\
    \ Yellow, the Historical Society may have held onto them. Perhaps there\
    \ you will find answers to the questions that burn in your mind."
  ]

sebastiensInformation :: Message
sebastiensInformation = FlavorText
  (Just "Sebastien's Information")
  [ "You recall what Sebastien told you during\
    \ the dinner party. The King in Yellow had come to Arkham several\
    \ decades ago, long before the Ward Theatre was built. According to\
    \ him, it isn’t surprising that few people remember—in fact, part of their\
    \ goal tonight was to bring The King in Yellow to a wider audience.\
    \ The Historical Society may have kept records pertaining to this earlier\
    \ production, especially if it was followed by events similar to what has\
    \ occurred tonight. Perhaps you can find some newspaper clippings or\
    \ other articles describing what happened in the past."
  ]

resolution1 :: Message
resolution1 = FlavorText (Just "Resolution 1") [""]

resolution2 :: Message
resolution2 = FlavorText (Just "Resolution 2") [""]

resolution3 :: Message
resolution3 = FlavorText (Just "Resolution 3") [""]
