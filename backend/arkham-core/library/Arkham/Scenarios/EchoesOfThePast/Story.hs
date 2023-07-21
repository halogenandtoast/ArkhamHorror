module Arkham.Scenarios.EchoesOfThePast.Story where

import Arkham.Prelude

import Arkham.Message

intro :: FlavorText
intro =
  FlavorText
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

sebastiensInformation :: FlavorText
sebastiensInformation =
  FlavorText
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

noResolution :: FlavorText
noResolution =
  FlavorText
    ( Just
        "If no resolution was reached (each investigator resigned or was defeated)"
    )
    [ "You barely escape the building with your body and mind intact, and flee to safety. Read Resolution 4."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "There are no coincidences when it comes to The\
      \ King in Yellow. There is no doubt in your mind that the object\
      \ you’ve found is important. You decide to take it with you before\
      \ continuing your investigation. The last record you find related to\
      \ the original production of The King in Yellow is a psychiatric\
      \ evaluation of one Daniel Chesterfield, a stagehand who lost his\
      \ wits after the final show. It seems he was admitted to the asylum\
      \ after the production ended. Perhaps he’s still there…"
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "This investigation would make anybody\
      \ paranoid. Attributing some greater meaning to everything you\
      \ find can be a dangerous proposition. You’re sure that this clasp\
      \ is meaningless, perhaps even a prop from the original play. You\
      \ leave it behind and continue your investigation. The last record\
      \ you find related to the original production of The King in\
      \ Yellow is a psychiatric evaluation of one Daniel Chesterfield, a\
      \ stagehand who lost his wits after the final show. It seems he was\
      \ admitted to the asylum after the production ended. Perhaps\
      \ he’s still there..."
    ]

resolution3 :: FlavorText
resolution3 =
  FlavorText
    (Just "Resolution 3")
    [ "The figure collapses to the ground, its warped,\
      \ melting body writhing in agony. It wheezes and cries out, a\
      \ yellow glow emanating from inside its mouth and behind\
      \ its eyes. “Daniel,” the thing says in its cracking voice. “Seek\
      \ Daniel... Daniel Chesterfield, the stagehand, he remembers!”\
      \ Deep inside the man’s words, there is another voice behind it,\
      \ whispering faintly in a melodic language. “Find him... He too\
      \ has spoken the oath... The oath that doomed us all...!” With\
      \ those final words, the skin melts off the man’s bones, and he\
      \ rattles to the floor."
    ]

resolution4 :: FlavorText
resolution4 =
  FlavorText
    (Just "Resolution 4")
    [ "The night’s events have left you exhausted.\
      \ You were unable to learn anything at the Historical Society’s\
      \ manor that could drive your investigation forward. Whoever\
      \ those intruders were, they must have been looking for the same\
      \ information you were. Distraught, you find your way to the\
      \ closest bed you can find—in Ma’s Boarding House, not too\
      \ far from the Historical Society. Dawn breaks as you reach the\
      \ boarding house, the front doorknob and ferns on the porch\
      \ covered in dew. You are lucky enough to get a vacant room for\
      \ the day, and soon you are fast asleep."
    , "Suddenly, you are falling. Above and all around you, a\
      \ blinding radiance flares like an inferno. Your head twinges\
      \ with pain and your vision swims with brilliant colors. Your\
      \ skin is singed by the flames. Below you, the abyss opens\
      \ and swallows you whole. You then find yourself in a dank,\
      \ windowless cell, filled with a putrid stench. A man is huddled\
      \ in a corner of the room, shivering from the cold, or from the\
      \ hopelessness of his situation, or perhaps both. “No mask,” he\
      \ mutters over and over, “no mask, no mask...” You take a few\
      \ tentative steps forward, but before you can reach him, the\
      \ metal slot on his cell door slides open, and you hear an orderly\
      \ call to him. “Daniel, it’s time,” he says. “Daniel, wake up.\
      \ Wake up, Daniel.” Then, you awaken."
    ]
