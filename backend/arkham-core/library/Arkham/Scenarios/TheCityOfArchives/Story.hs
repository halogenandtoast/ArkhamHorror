module Arkham.Scenarios.TheCityOfArchives.Story where

import Arkham.Prelude

import Arkham.Message

intro1 :: FlavorText
intro1 =
  FlavorText
    (Just "Intro 1")
    [ "You remember very little of the next few days. Your consciousness fades in and\
      \ out as your body grips with exhaustion, and your mind feebly attempts to\
      \ comprehend what exactly has happened to you."
    , "The inhabitants of this place are the same creatures you saw Alejandro\
      \ consorting with: immense corrugated cones of clammy flesh, with four thick\
      \ limbs extending from the top. They are horrifying to look upon, but they do not\
      \ harm you as you expected they might. Your first few days are spent restrained\
      \ and questioned by the creatures, several of which are somehow able to\
      \ communicate in English. Others reproduce your language using an odd futuristic\
      \ machine, played like a musical instrument. You are reminded strangely of the\
      \ jazz piano at the Nightingale. It is only then that you realize your body has\
      \ been altered. You have no hands or fingers to play the keys, no legs to walk\
      \ upon, no lungs to scream out. You are one of them now, one of the creatures\
      \ that has taken you captive, and your original body is nowhere to be seen."
    , "Despite your captivity, the creatures do not seem intent on harming you.\
      \ Instead, they merely interview you, asking all manner of questions about a\
      \ broad range of topics: your society, your profession, your technology, your\
      \ ecosystem, your human body. Their thirst for knowledge is unyielding, unending.\
      \ Their questions range from the mundane to the complex. They implore you to\
      \ write copiously in your own language, taking anything you write to store away\
      \ in their vast archives. You are informed that you will be able to roam the city\
      \ freely if you comply."
    ]

intro2 :: FlavorText
intro2 =
  FlavorText
    (Just "Intro 2")
    [ "You cooperate with the creatures, hoping to earn your freedom. You are sick of\
      \ captivity, and perhaps you will be able to find a way home if you are able to\
      \ roam the city at will. The creatures seem pleased that you are answering their\
      \ questions, though their mannerisms are still completely alien to you. Days turn\
      \ to weeks, perhaps even longer. It is difficult to tell. Finally, one of the\
      \ creatures informs you that you are free to roam the halls of the city, with\
      \ several restrictions: You are not allowed to do harm to your new body, for it\
      \ belongs to another of their race, and you are not allowed to leave the city.\
      \ Your restraints are lifted, and the creature watches with keen interest as you\
      \ get used to your conical body."
    ]

intro3 :: FlavorText
intro3 =
  FlavorText
    (Just "Intro 3")
    [ "These creatures abducted you and even stole you from your own body. You refuse\
      \ to give your captors even a single answer. You’d spit in their faces if you\
      \ could—if you knew how to spit with such body, and if you knew which one of the\
      \ creatures’ four distensible appendages was a face.  Breaking free of your\
      \ restrains is a hopeless endeavor, but you struggle to escape regardless. Your\
      \ efforts do not go unnoticed, and one of the creatures is assigned to watch you.\
      \ It seems more concerned with preventing you from harming your body than\
      \ preventing you from escaping. After many days of captivity, you find a way to\
      \ use this to your advantage. You struggle so hard against your restraints that\
      \ they pierce into your leathery, cold flesh, drawing out a bit of viscous\
      \ slime-blood. Your guard quickly apporoaches, clicking its claws together\
      \ repeatedly. It undoes your restraints, and the moment it does, your claw\
      \ appendages strike at what you believe to be the creature’s throat. It collapses\
      \ to the floor, writhing in pain."
    ]

noResolution :: FlavorText
noResolution =
  FlavorText
    (Just "No Resolution")
    [ "“Alejandro, what are you—?” You suddenly cry out. A nurse stands over you,\
      \ peering into your eyes with a small light. You push the nurse away and rise to\
      \ your feet, suddenly puzzled and disoriented. Despite the nurse’s protests, you\
      \ wobble feebly out of the room. You are in a hospital, its white halls smelling\
      \ faintly of disinfectant and other chemicals. Just moments prior, hadn’t you\
      \ been exploring some kind of underground cavern?... You cannot recall."
    , "“Excuse me? Come back, you must rest!” The nurse calls out to you, gripping\
      \ your forearm and pulling you back to your cot. She examines you closely while\
      \ you recover from the shock of being transported so far in so short a time. “Who\
      \ is Alejandro?” she asks, and you struggle to recall. The name doesn’t ring a\
      \ bell."
    , "You ask the nurse how you came to be here, wondering why she isn’t surprised at\
      \ your sudden appearance. “Why, you admitted yourself to St. Mary’s just last\
      \ night. Do you not remember?” You have no memory of this, and you are starting\
      \ to grow concerned. “You were acting a little strangely, if I may be so bold.\
      \ And you spoke in a strange fashion, almost as if English weren’t your first\
      \ language. But now I see that you probably just drank a little bit too much.”\
      \ She notices your questioning expression and smiles. “Don’t worry. We get that\
      \ kind of thing quite often these days, now that all the bars and pubs are on the\
      \ up-and-up again.”"
    , "You examine your face in the mirror. You are tired and older than you remember.\
      \ Something important lies on the very edge of your memory, fuzzy and blurred. It\
      \ takes about a minute before the significance of the nurse’s last statement\
      \ catches up to you. Turning back toward the nurse, you ask what year it is,\
      \ terrified of the answer."
    , "“Why, 1934 of course. My, you must have had a heck of a night!” She chuckles."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "You awaken on the cold stone surface of the cavern floor, your mind dizzy\
      \ from its journey through time and space."
    ]
