module Arkham.Scenarios.WhereDoomAwaits.Story
where

import Arkham.Prelude
import Arkham.Text

intro :: FlavorText
intro =
  FlavorText
    (Just "Scenario VI: Where Doom Awaits")
    [ "You awaken to the sound of screeching. Fearing\
      \ the worst, you grab your equipment and head\
      \ out into the streets of Dunwich. As soon as you\
      \ step outside, you sense a foulness in the cold night\
      \ air: an awful, pungent smell that can scarcely\
      \ be described and a heaviness to the atmosphere\
      \ that makes it difficult to breathe. The citizens of\
      \ Dunwich have sealed their doors, and the town feels quiet and lonesome.\
      \ In the distance, a faint glow emanates from a hilltop above the village.\
      \ You know of this hill from both your interactions with Zebulon and\
      \ Armitage’s records. It is called Sentinel Hill. The tales speak of satanic\
      \ rites being performed there—rites in which great ritual-pyres light up\
      \ the night sky while the ground rumbles furiously below."
    , "Flocks of whippoorwills perch on the rooftops of the village around you,\
      \ watching ominously as you climb inside Zebulon’s old and beat-up truck.\
      \ As you drive towards Sentinel Hill, more screeching fills the sky with an\
      \ awful pitch that is painful to your ears. Everything you have read about\
      \ and experienced in Dunwich has led to this. If the foul ritual Seth seeks\
      \ to perform has anything to do with what Armitage and his colleagues\
      \ prevented several months back, it involves the favor of an ancient\
      \ creature—Yog‑Sothoth. Failing to stop this ritual may spell doom...not\
      \ only Dunwich, but for the entire world."
    ]

introPart1 :: FlavorText
introPart1 =
  FlavorText
    Nothing
    [ "The path leading up Sentinel Hill is narrow and too torn up for\
      \ Zebulon’s truck, so you park at the base of the hill and prepare to make\
      \ the rest of the trip on foot. Just then, you notice that you are not alone.\
      \ Several men and women emerge from the woods behind you, brandishing\
      \ firearms and lining you up in their sights. You raise your hands and brace\
      \ for the worst. “Wait,” one of them says, raising his hand to the others. “I\
      \ recognize you from the Clover Club.” He grins toothily and lowers his\
      \ weapon. “Naomi sends her regards.”"
    , "Curious, you ask what the gangsters are doing here. “Ms. O’Bannion had\
      \ us investigate the attack on her fiancé’s club,” he explains. “Turns out\
      \ there were some men in Arkham behind the whole thing. Some Bishop\
      \ fellow and his lackeys. We tailed them all the way to this dump.” Before\
      \ he can explain further, the all-too-familiar ratta-tat of a tommy gun\
      \ echoes across the hill. “That’ll be Vinny. Come on, boys!” He beckons to\
      \ the others to follow and runs up the hill. Shaking your head, you do the\
      \ same. These mobsters don’t know what they’re getting into."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "The poorly bound tome appears to be the\
      \ written records of Old Whateley, the man who taught Wilbur\
      \ the ancient secrets of sorcery. You find a passage describing a\
      \ place outside of time and space, where worlds converge and\
      \ Yog-Sothoth dwells. Only by reaching this nexus at the edge of\
      \ reality can you unmake the tear that has split open the world.\
      \ Feeling as if you may be going to your doom, you muster a\
      \ final ounce of courage and step into the gate."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "The sorcerers from Dunwich, seeking arcane\
      \ power from beyond this realm, have accomplished what\
      \ Wilbur and Old Whateley could not. Through blood sacrifice\
      \ and indescribable experiments, the dark power the sorcerers\
      \ sought is now within their reach. However, they will never get\
      \ the chance to truly wield this power. In beseeching Wilbur’s\
      \ father for knowledge, they have drawn the creature forth\
      \ from its extradimensional realm. Yog-Sothoth emerges from\
      \ the open rift above Sentinel Hill, blotting out the sky and\
      \ enveloping the world. Now it has come to Earth, and it rules\
      \ where humanity once tread."
    ]
