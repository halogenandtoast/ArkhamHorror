module Arkham.Scenarios.LostInTimeAndSpace.FlavorText
where

import Arkham.Prelude

import Arkham.Text

intro :: FlavorText
intro =
  FlavorText
    (Just "Scenario VII: Lost in Time and Space")
    [ "Passing through the gate is unlike anything you’ve\
      \ ever experienced. You feel your body twisting and\
      \ distorting, churning through realities as the gate\
      \ pulls you to its ultimate destination—a place\
      \ beyond the scope of human imagination."
    , "Suddenly, all is quiet and the chaos of the\
      \ journey is replaced with a sense of solitude and dread. You are in an\
      \ unfathomable place, vast beyond your ability to reason and utterly\
      \ alien besides. The landscape is surreal and strange, the architecture\
      \ impossible. You are so far from home that home has become a\
      \ threadbare dream you can barely recall. Even should you find a way out\
      \ of this awful place, you may never be the same again."
    ]

investigatorDefeat :: FlavorText
investigatorDefeat =
  FlavorText
    Nothing
    [ "Where did you come from? Why are\
      \ you here? Are you dreaming, or is this place real? Now that\
      \ you think about it, haven’t you been here before? Or perhaps\
      \ you’ve been here all along. Now you remember. This is your\
      \ real home. The path you now walk is but one ledge, with many\
      \ more below. You only have to fall and you will be where you\
      \ belong. One more step…"
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "Lying on your back in a patch of wet grass, you\
      \ find yourself staring longingly at the night sky. Somehow, you\
      \ are once again atop Sentinel Hill, unable to recall exactly how\
      \ you got here. You are mesmerized by the night sky. Seconds\
      \ become minutes, and minutes become hours. Eventually,\
      \ you are found and lifted to your feet by a group of Dunwich\
      \ citizens. “What happened? What are ya doin’ here?” they ask\
      \ you, frightened but curious. You can’t seem to find the right\
      \ words to describe the events that occurred beyond the gate…if\
      \ they ever truly occurred. There doesn’t appear to be any trace\
      \ of Seth Bishop, of the creatures you fought earlier, or of the\
      \ phantasmal and otherworldly gate. But every time you sleep,\
      \ you dream—and when you dream, it all comes rushing back."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "Several of the villagers from Dunwich heard\
      \ the ruckus on Sentinel Hill and went to investigate. What they\
      \ found there answered none of their questions."
    , "“What d’you think happened?” a frightened Curtis Whateley\
      \ asks as they examine the hilltop. The other villagers shake\
      \ their heads, unable to say. “Last thing I saw, the sky’d open’d\
      \ up an’ there was a bright flash,” one of them says, looking up at\
      \ the starry sky from the top of Sentinel Hill."
    , "“Those Arkham fellas ain’t nowhere to be seen,” Curtis adds,\
      \ looking down at the cracked stone altar. “Not even of a hint of\
      \ ‘em. Mr. Bishop and his pals, now, that’s a diff’rent story.” The\
      \ young man motions toward several corpses on the ground."
    , "“Think we should get ahold of those coppers outta Aylesbury?”"
    , "“Why, so they can laugh at us again?” Curtis spits. “They ain’t\
      \ never gonna believe us ‘bout all this anyway.” He glances at\
      \ the stone altar, fidgeting nervously. “Better t’ forget about the\
      \ whole thing. C’mon, let’s bury the dead and get outta here.”"
    ]

resolution3 :: FlavorText
resolution3 =
  FlavorText
    (Just "Resolution 3")
    [ "The creature erupts in a cosmic fury of sound,\
      \ color, and distorted space, hurling you back and away from it.\
      \ You watch in horror as one of its arms tears through the fabric\
      \ of the world, and its amorphous shape funnels through the\
      \ rift, pulling the threads of the world along with it. You try to\
      \ cling to something, but you are inexorably sucked into the rift.\
      \ You feel as if your body is stretching and your mind is being\
      \ crushed. Then, everything goes black."
    ]

resolution4 :: FlavorText
resolution4 =
  FlavorText
    (Just "Resolution 4")
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
