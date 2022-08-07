module Arkham.Scenarios.TheLastKing.Story where

import Arkham.Prelude

import Arkham.Message

intro :: Message
intro = FlavorText
  (Just "Scenario II: The Last King")
  [ "If anyone has the answers to the questions that burn in your mind, it’s\
    \ the cast and crew of The King in Yellow. With no other leads to pursue,\
    \ you put on your best clothes and head to 1452 Atlantic Avenue, where a\
    \ woman named Constance Dumaine is hosting an event in celebration of\
    \ the play’s one-night engagement in Arkham."
  ]

resolution1 :: Message
resolution1 = FlavorText
  (Just "Resolution 1")
  [ "The brisk autumn air embraces you as you exit\
    \ the manor. There is no doubt in your mind that the cast and\
    \ crew of The King in Yellow have become affected by madness.\
    \ Perhaps it’s getting to you, as well. You feel an encroaching\
    \ darkness, a presence in your mind not unlike the gaze of the\
    \ Stranger. Startled, you peer about the front yard for the first\
    \ time since escaping the manor. The scene is…different. The\
    \ front windows are not smashed as they were when you first\
    \ approached. The trail of blood you had noticed on the porch\
    \ has been wiped clean, and instead of the disturbingly warped\
    \ music you had heard upon entering, the soothing tones of slow\
    \ jazz drift out from the courtyard."
  ]

resolution2 :: Message
resolution2 = FlavorText
  (Just "Resolution 2")
  [ "“Excuse me, but it’s very late,” you hear a\
    \ server say as he gently taps you on your shoulder. You realize\
    \ that you are sitting on a couch in the manor’s living room, and\
    \ that you have been asleep for some time. The party appears to\
    \ be winding down. No music fills the halls, the food is all but\
    \ gone, and only a few guests remain. “Perhaps you would like\
    \ for me to get your coat?” the server asks with a trained smile.\
    \ You wobble as you rise to your feet, leaning against the couch’s\
    \ armrest. Your head pounds with a dizzying intensity, and\
    \ your vision is spotted. You insist that you are fine, and begin\
    \ walking toward the foyer."
  , "You no longer see any of the guests you were searching for\
    \ earlier, not even the hostess, Mrs. Dumaine. All traces of\
    \ the madness and horror you’ve experienced are gone. Even\
    \ the oddities you witnessed upon entering the manor have\
    \ vanished—the signs of struggle, the broken windows, the\
    \ blood trail on the porch… Every piece of evidence has been\
    \ erased. But you still remember the night’s events, and in your\
    \ memory you will find your answers."
  ]

resolution3 :: Message
resolution3 = FlavorText
  (Just "Resolution 3")
  [ "Several days later, you find yourself\
    \ reminiscing about the party you attended at 1452 Atlantic\
    \ Avenue. What a roaring good time you had! And yet, your\
    \ memory is hazy. You can’t help but feel you are forgetting\
    \ something important. Something about the play you’d\
    \ watched earlier that night—The King in Yellow. That\
    \ nagging sensation pursues you in every waking moment. Try\
    \ as you might to recall the night’s events in full, there remains\
    \ a gaping hole in your memory. As you strain to remember,\
    \ your concern grows, almost to the point of obsession. Deciding\
    \ that you absolutely must know what occurred that night,\
    \ you head toward the manor of the Historical Society in\
    \ Southside. Perhaps there you can learn more about The King\
    \ in Yellow play…"
  ]
