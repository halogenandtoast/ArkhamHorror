module Arkham.Scenarios.CurseOfTheRougarou.FlavorText where

import Arkham.Prelude

import Arkham.Text

intro :: FlavorText
intro = FlavorText
  (Just "Terror Grips New Orleans!")
  [ "Minnie Klein, your contact at the Arkham Advertiser, has slipped\
    \ you a draft of the article over a cup of coffee at Velma's Diner. It\
    \ would have gone to print had Doyle Jeffries, the lead editor, not\
    \ scoffed at the concept. \"I believe his exact words were, 'I ain't\
    \ printing the ravings of some Voodoo lunatic and passing it off as news,'\"\
    \ she explained. From the sly grin spreading across her face, you could\
    \ tell she smelled a story."
  , "The headline was sensationalist. Three killings, in nine days was\
    \ enough to spook a town, sure. But you doubt all of New Orleans is\
    \ gripped by terror, or even knows about the killings. Still, something\
    \ piqued your interest. \"Lady Esprit,\" the Voodoo priestess from the\
    \ article, claimed that a malign curse had taken root in the bayou."
  , "\"There's something to this, isn't there? I know that look,\"\
    \ Minnie said. You weren't sure. If Lady Esprit was right, this\
    \ \"roux-ga-roux\" wouldn't stop killing at three, that's for sure.\
    \ But curses? Wolf-people? How could such things be real? Only one way\
    \ to find out. You put on your coat and head for the Northside Station..."
  ]

resolution1 :: FlavorText
resolution1 = FlavorText
  (Just "Resolution 1")
  [ "Somehow, you manage to make it back safely before daybreak,\
    \ resting until late in the afternoon. It isn't until you seek\
    \ out Lady Esprit the next day that you realize who last night's\
    \ victim was. With a heavy heart and an unshakable dread, you\
    \ choose to bury her body instead of contacting the authorities\
    \—the less people who delve this deep into the bayou, the better."
  ]

resolution2 :: FlavorText
resolution2 = FlavorText
  (Just "Resolution 2")
  [ "The creature gives a pitiful wail as dark miry blood oozes from\
    \ its wounds. By the time its body collapses into the mud, it has\
    \ transformed back into its original form—the form of a yound dark\
    \-skinned man, his expression twisted in agony. You bring his body\
    \ back to Lady Esprit and she works her strange magic, removing the\
    \ stain of the curse from the land. \"Call on me should you ever\
    \ need my help,\" the mysterious woman tells you."
  ]

resolution3 :: FlavorText
resolution3 = FlavorText
  (Just "Resolution 3")
  [ "Somehow, you have managed to quell the rage and bloodlust\
    \ of the curse within the creature, and in moments the shape\
    \ of a young, dark-skinned man stands before you, panting and\
    \ sweating. He seems to only just now understand everything\
    \ he's done, and agrees to flee to a secluded corner of the\
    \ earth where he can harm no one. However, the curse lives\
    \ on. He sees it in your eyes and grips your arm tightly.\
    \\"Don't let it take control,\" he warns. \"I was weak, but\
    \ you—I can tell you are strong. Control the curse as I could\
    \ not.\""
  ]
