module Arkham.Scenarios.TheGathering.Story where

import Arkham.Prelude

import Arkham.Message

theGatheringIntro :: FlavorText
theGatheringIntro = FlavorText
  (Just "Part I: The Gathering")
  [ "You and your partners have been investigating strange events taking place\
    \ in your home city of Arkham, Massachusetts. Over the past few weeks,\
    \ several townspeople have mysteriously gone missing. Recently, their\
    \ corpses turned up in the woods, savaged and half - eaten. The police and\
    \ newspapers have stated that wild animals are responsible, but you believe\
    \ there is something else going on. You are gathered together at the lead\
    \ investigator’s home to discuss these bizarre events."
  ]

noResolution :: FlavorText
noResolution = FlavorText
  Nothing
  [ "You barely manage to escape\
  \ your house with your lives. The woman from your parlor\
  \ follows you out the front door, slamming it behind her. “You\
  \ fools! See what you have done?” She pushes a chair in front of\
  \ the door, lodging it beneath the doorknob. “We must get out\
  \ of here. Come with me, and I will tell you what I know. We\
  \ are the only ones who can stop the threat that lurks beneath\
  \ from being unleashed throughout the city.” You’re in no state\
  \ to argue. Nodding, you follow the woman as she runs from\
  \ your front porch out into the rainy street, toward Rivertown."
  ]

resolution1 :: FlavorText
resolution1 = FlavorText
  (Just "Resolution 1")
  [ "You nod and allow the red-haired woman to\
  \ set the walls and floor of your house ablaze. The fire spreads\
  \ quickly, and you run out the front door to avoid being caught\
  \ in the inferno. From the sidewalk, you watch as everything\
  \ you own is consumed by the flames. “Come with me,” the\
  \ woman says. “You must be told of the threat that lurks below.\
  \ Alone, we are surely doomed…but together, we can stop it.”"
  ]

resolution2 :: FlavorText
resolution2 = FlavorText
  (Just "Resolution 2")
  [ "You refuse to follow the overzealous woman’s\
  \ order and kick her out of your home for fear that she will set\
  \ it ablaze without your permission. “Fools! You are making\
  \ a grave mistake!” she warns. “You do not understand the\
  \ threat that lurks below…the grave danger we are all in!”\
  \ Still shaken by the night’s events, you decide to hear the\
  \ woman out. Perhaps she can shed some light on these bizarre\
  \ events…but she doesn’t seem to trust you very much."
  ]

resolution3 :: FlavorText
resolution3 = FlavorText
  (Just "Resolution 3")
  [ "You run to the hallway to try to find a way to\
  \ escape the house, but the burning-hot barrier still blocks your\
  \ path. Trapped, the horde of feral creatures that have invaded\
  \ your home close in, and you have nowhere to run."
  ]
