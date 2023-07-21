module Arkham.Scenarios.TheDevourerBelow.Story where

import Arkham.Prelude

import Arkham.Text

intro :: FlavorText
intro =
  FlavorText
    (Just "Part III: The Devourer Below")
    [ "After a frantic nighttime search throughout Arkham, you have tracker\
      \ down and questioned several members of the cult. Your findings are\
      \ disturbing: they claim to worship a being known as Umôrdhoth, a\
      \ monstrous entity from another realm."
    , "You are able to confirm much of Lita’s story: the cult is agitated over\
      \ the destruction of a ghoul lair. However, a surprising detail also turns\
      \ up: the one who invaded the lair and set this night’s events in motion\
      \ was none other than Lita Chantler herself! You are not sure why this\
      \ important detail was omitted from Lita’s story—did she tell you only\
      \ as much as was necessary to draw you into her conflict? But in another\
      \ light, she seems to be fighting to protect the city of Arkham from a\
      \ terrible menace."
    , "The final piece of the puzzle was found written in a journal possessed by\
      \ one of the cultists. It describes a dark ritual to be performed deep within\
      \ the woods south of Arkham, this very night. According to the journal,\
      \ the ritual’s completion will open a gate and bring forth the cult’s dark\
      \ master into this world. “If the cult is not stopped,” Lita warns, “there is\
      \ a possibility that Umôrdhoth’s vengeance will consume all in its path.”\
      \ Frightened but determined to stop the ritual, you head into the woods…"
    ]

noResolution :: FlavorText
noResolution =
  FlavorText
    Nothing
    [ "Too frightened to face her fate, Lita flees\
      \ into the night. She realizes that she has failed and Umôrdhoth’s\
      \ vengeance will pursue her wherever she goes. The creature’s\
      \ tendrils spread throughout the city of Arkham, searching for\
      \ her. It lurks in the darkness of every corner, tugging at the seams\
      \ of reality. But Lita is nowhere to be found, so the creature dwells\
      \ in the shadows to this day, searching…killing"
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "You have managed to prevent the cult from\
      \ summoning its master. Although you’re unsure what would\
      \ have happened had the cult succeeded, you’re relieved that—at\
      \ least for the time being—Arkham is safe. You capture as many\
      \ cultists as you can find, but very few townspeople believe your\
      \ tale. Perhaps it was all in your head, after all."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "Through force of arms and strength of will,\
      \ you are somehow able to harm Umôrdhoth enough to send it\
      \ reeling back to the dimension from which it emerged. Warmth\
      \ and light return to the woods as the void-like mass is sucked in\
      \ upon itself, vanishing in an instant. You aren’t sure if a being\
      \ such as this can be killed, but for the time being it seems to have\
      \ retreated. As their master vanishes, the ghouls nearby climb\
      \ into the open pit below, fleeing with terrible cries and shrieks.\
      \ You have stopped an evil plot, but the fight has taken its toll on\
      \ your body and mind. Worse, you can’t help but feel insignificant\
      \ in the face of the world’s mysteries. What other terrors exist in\
      \ the deep, dark corners of reality?"
    ]

resolution3 :: FlavorText
resolution3 =
  FlavorText
    (Just "Resolution 3")
    [ "In the face of this horror, you don’t believe there\
      \ is anything you can do to stop it. You have but one hope if you\
      \ are to survive. You turn on Lita and throw her at the terrible\
      \ monstrosity, watching in dread as its swirling void-like mass\
      \ consumes her. She cries out in torment as the life is sucked from\
      \ her body. “Umôrdhoth…Umôrdhoth…” the cultists chant.\
      \ Lita Chantler vanishes without a trace. For a moment, you\
      \ fear that the creature will now turn on you, but you hear one of\
      \ the cultists say, “Umôrdhoth is a just god who claims only the\
      \ guilty and the dead. Go, and you shall be spared.” The swirling\
      \ mass vanishes, and warmth and light return to the woods. The\
      \ cultists slink away, leaving you alive. Lita’s last moments are\
      \ forever etched upon your memory."
    ]
