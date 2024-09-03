module Arkham.Scenarios.CarnevaleOfHorrors.FlavorText
where

import Arkham.Prelude

import Arkham.Text

intro :: FlavorText
intro =
  FlavorText
    (Just "The Carnevale is Coming...")
    [ "\"Look,\" Sheriff Engel insists, \"I know it sounds crazy, but that's\
      \ all there is to it.\" He sighs and sits back down, pouring a cup of joe\
      \ for you and one for himself. \"A dame in Uptown spotted a cracked egg\
      \ wearing this mask and holdin' a bloody butcher's cleaver,\" he says,\
      \ motioning to the black leather mask sitting on his desk. It has a comically\
      \ long nose and a strange symbol scrawled in yellow on its forehead. \"So, she\
      \ calls it in. My boys and I picked him up on the corner of Saltonstall &\
      \ Garrison.\" The sheriff\'s jaw clenches and his brows furrow as he recounts\
      \ the story. \"Fella did nothing but laugh as we slapped the bracelets on him.\
      \ Called himself Zanni. Said nothing except the 'carnival is coming,' whatever\
      \ the hell that meant. Wasn't until the next day we found the victim's body.\
      \ Defense wanted him in a straitjacket. We were happy to oblige.\""
    , "There isn't much time to spare. If your research is right, there is more to\
      \ this case than meets the eye. This \"Zanni\" wasn't talking about Darke's\
      \ Carnival, but rather, the Carnevale of Venice, which begins just before the\
      \ next full moon..."
    ]

noResolution :: FlavorText
noResolution =
  FlavorText
    (Just "No resolution")
    [ "You sputter awake as an oar gently taps your shoulder. \"Tutto bene?\"\
      \ The gondolier holding the oar says with a concerned expression. You nod\
      \ and drag yourself onto the docks from his gondola, drenched and aching\
      \ all over. The city is devastated. Most of the boats in the canal are\
      \  wrecked, and the streets are covered not in confetti, but in blood..."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "The city is still recovering from the events during the eclipse. With\
      \ nearly all evidence of the creature melted away by the hot sun, many\
      \ attribute the violence during the Carnevale to local crime lord Cascio\
      \ Di Boerio and his crew. Those that know the truth know better than the\
      \ speak of the elder creature that lives in Laguna Veneta. With any luck,\
      \ its name will never be spoken again."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "The creature recoils as globules of its jelly-like flesh rip and tear\
      \ from its body, splashing into the lagoon. It makes no sound as its torn\
      \ body sinks into the depths. The chanting in the city plunges into mournful\
      \ silence. As you return it to the canal-side streets, black feathers fall\
      \ from the sky where bright confetti once fluttered. You can only wonder how\
      \ long it will take for the creature to recover."
    ]

sacrificesMade :: FlavorText
sacrificesMade =
  FlavorText
    (Just "Sacrifices Made")
    [ "Too many lives were lost during the eclipse to stop the machinations of\
      \ Cindathqua's servants. The beast has been fed, its minions empowered. You find \
      \ yourself hoping you don't live long enough to see the fallout of your failure."
    ]

abbessSatisfied :: FlavorText
abbessSatisfied =
  FlavorText
    (Just "Abbess Satisfied")
    [ "\"Grazie mille - thank you for all your help,\" Allegria says as you return\
      \ to the basilica. \"Thanks to you, there were few casualties. I shudder to think\
      \ what might have happened had you not arrived. Should you ever require assistance,\
      \ please do not hesitate to ask."
    ]
