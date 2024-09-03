module Arkham.Scenarios.TheWagesOfSin.Story where

import Arkham.Prelude

import Arkham.Text

intro :: FlavorText
intro =
  FlavorText
    (Just "The Wages of Sin")
    [ "Ever since your encounter with the ghost in the Witch House, you have been\
      \ forced to reconsider everything you know about life and death. Keziah Mason\
      \ died over two hundred years ago, but her spirit remains in the spectral\
      \ mist—the same mist you encountered in Josef Meiger’s manor. One question\
      \ lingers in your mind as you consider the events that have transpired: Does\
      \ Keziah’s spirit linger because of the mist, or is the mist caused by her\
      \ presence? Which is the disease, and which the symptom?"
    , "The more you delve into Arkham’s history of witchcraft and persecution, the\
      \ more restless you become. Night after night, you are assaulted by terrible\
      \ dreams—dreams of death and decay, of guilt and sin. There is an evil that\
      \ dwells in this city, an evil that has long remained hidden in the darkness of\
      \ Arkham’s past. What’s worse, your inquiry has reached an impasse. You believe\
      \ that if you can find the coven of witches you saw in the woods that fateful\
      \ night, you might be able to learn more about Keziah Mason or of this unspoken\
      \ evil."
    , "As you go through your morning routine, your eyes drift across one of the\
      \ smaller headlines of today’s newspaper: “Ghost Sightings on Hangman’s Hill?\
      \ Residents of Uptown claim to have seen human shapes in the thick fog emanating\
      \ from Hangman’s Brook...” It couldn’t be a coincidence, could it?"
    , "Then the realization dawns on you—if it is witches you seek, the only place you\
      \ can be certain to find them is six feet underground, in the place where\
      \ professed witches were executed all those years ago. Searching for specters in\
      \ a graveyard may not be your safest bet, but you’re not sure one can stay safe\
      \ in this city anymore."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "Rays of faded sunlight stream through the haze, and the unnatural mist finally\
      \ recedes. As it does, Hangman’s Hill returns to its original state. No other\
      \ spiteful ghosts emerge to haunt you, and the witches who summoned them have\
      \ retreated into the shadows once more. You hope that you have done enough to\
      \ prevent the rest of Arkham to succumbing to this horror."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "Resigned to your fate, you fall to your knees. The mist parts. Time slows to a\
      \ crawl as the watcher emerges. Its spectral form glides along the ground,\
      \ approaching with the slow surety of a ticking clock. You close your eyes,\
      \ unwilling to look upon your killer. Your fingers dig into your palm, and you\
      \ brace yourself."
    , "The moment passes."
    , "When you open your eyes again, the creature is gone, along with the spectral\
      \ mist."
    , "...Have you been spared?"
    ]
