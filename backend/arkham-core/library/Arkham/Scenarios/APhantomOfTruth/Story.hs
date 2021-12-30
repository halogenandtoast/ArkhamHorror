module Arkham.Scenarios.APhantomOfTruth.Story where

import Arkham.Prelude

import Arkham.Message

intro1 :: Message
intro1 = FlavorText
  (Just "Scenario IV: The Unspeakable Oath")
  [ "Several months have passed since The\
    \ King in Yellow came to the Ward Theatre. You\
    \ and your companions have been investigating\
    \ the strange incidents that followed in its wake.\
    \ What you have found makes you question the wisdom of continuing\
    \ your investigation, but the truth calls out to you, and you cannot help\
    \ but answer. It seems you weren’t the first to piece together the strange\
    \ happenings surrounding The King in Yellow. Another group of\
    \ investigators had been researching these events as well. Just a matter\
    \ of days after the performance in Arkham, they were admitted to the\
    \ asylum, ranting about the King’s return and the “monsters” that had\
    \ attacked them. Somehow this doesn’t surprise you. Their fate makes\
    \ you all the more certain you are on the right trail... And all the more\
    \ certain that you should stop while you still have the chance."
  , "You’ve found the records those investigators kept before their\
    \ incarceration, and picked up the trail where they left off. They had\
    \ interviewed some of the cast and crew of The King in Yellow, and\
    \ discovered several pieces of vital information. Their research also\
    \ contains notes about their discoveries in Arkham’s Historical Society,\
    \ and accounts from their time spent in the asylum, including interactions\
    \ with a patient named “Daniel Chesterfield.” After a long night of\
    \ compiling and comparing notes, you fall into a deep slumber. In your\
    \ dreams, you are subjected to visions of Carcosa—its black stars, twin\
    \ suns, shattered moons, and twisted spires."
  ]

intro2 :: Message
intro2 = FlavorText
  (Just "Scenario IV: The Unspeakable Oath")
  [ "It has been several weeks since the events in the asylum, and\
    \ you are still no closer to the truth about The King in Yellow and\
    \ Carcosa. You have scoured the city for signs of the others Daniel had\
    \ mentioned—the ones who are “opening the path to Carcosa”—but\
    \ found nothing. Either the trail has gone cold, or they’re no longer in\
    \ Arkham. Perhaps Daniel truly was insane, and you are only following\
    \ him deeper down the rabbit hole. Every night, you toss and turn as you\
    \ are subjected to vivid dreams of Carcosa—its black stars, twin suns,\
    \ shattered moons, and twisted spires."
  ]

dream1 :: Message
dream1 = FlavorText
  (Just "Dream 1")
  [ "You fall through the empty abyss of Hali. Creatures of\
    \ unknown and impossible origin lurk just beyond the darkness of your\
    \ sight. Hastur looms above you, magnificent and yet bound in his\
    \ prison of madness, manipulating your torturous descent with a sole\
    \ outstretched arm."
  ]

dream2 :: Message
dream2 = FlavorText
  (Just "Dream 2")
  [ "Constance takes your hand and pulls you onto the polished\
    \ dance floor. “Come now, don’t be shy. Tonight is a night for dancing!\
    \ For celebration!”"
  ]

dream3 :: Message
dream3 = FlavorText
  (Just "Dream 3")
  [ "“Ah, if it isn’t our esteemed guest,” the man with the\
    \ mustache and the silver-handled cane says. He grabs a bottle of wine\
    \ from the nearby countertop and pours you a glass. “I hear you have\
    \ taken quite the interest in our little production,” he says with a smile.\
    \ “Tell me: how are you enjoying Act 2, so far?”"
  ]

dream4 :: Message
dream4 = FlavorText
  (Just "Dream 4")
  [ "Smoke and embers float upwards into the starless night\
    \ sky. The screams of burning creatures fill you with a horrid sense of\
    \ accomplishment. They almost sound human, you consider. But you\
    \ know that isn’t true."
  ]

dream5 :: Message
dream5 = FlavorText
  (Just "Dream 5")
  [ "You peer in the mirror, and your reflection gives you a\
    \ curious glance. “Wait a second, this isn’t Dream 1,” you say."
  ]

dream6 :: Message
dream6 = FlavorText
  (Just "Dream 6")
  [ "The lights of the theatre dim and a spotlight shines on the\
    \ stage. “Welcome, ladies and gentleman!” the creature exclaims. Its many\
    \ tentacles reach across the stage, up into the rafters, and throughout the\
    \ aisles. It tears the curtains down, and tattered red cloth falls over its\
    \ bulbous form."
  ]

dream7 :: Message
dream7 = FlavorText
  (Just "Dream 7")
  [ "You find yourself inside the Arkham Police Department,\
    \ desperately trying to explain to the desk sergeant what happened in the\
    \ Ward Theatre. He scoffs and refuses to believe you. “We know what\
    \ you’re really up to,” he says, and claps a pair of handcuffs over your\
    \ wrists. “You’re under arrest for larceny. Boys, take ‘em away.” Several\
    \ other cops flank you and prevent any escape. Your protests go unheard\
    \ as you are firmly escorted down a flight of concrete stairs and hurled\
    \ into a cold jail cell. “Maybe some time in lockup will change your story,”\
    \ the desk sergeant shouts. He slams the door behind him, leaving you\
    \ with your spiraling thoughts. How did they find out? How could they\
    \ possibly know?"
  ]

dream8 :: Message
dream8 = FlavorText
  (Just "Dream 8")
  [ "You peer in the mirror, and the Stranger peers back at you.\
    \ His gaze drills into your mind. The mirror shatters."
  ]

dream9 :: Message
dream9 = FlavorText
  (Just "Dream 9")
  [ "You chase the Stranger through dark, chilly passageways\
    \ composed of hundreds of thousands of skeletal bodies. Skulls watch\
    \ as you run, their mouths rattling a mocking taunt as you pass. The\
    \ Stranger reaches a solid wall of bone, and is grabbed by bony hands and\
    \ pulled into the wall. Their mocking laughs reach an awful crescendo."
  ]

dream10 :: Message
dream10 = FlavorText
  (Just "Dream 10")
  [ "You chase the Stranger up a steep staircase of broken\
    \ stone, a torrent of rain crashing upon your back as you run. Lighting\
    \ flashes in the distance. A vortex of swirling black clouds looms above\
    \ you, threatening to swallow the world whole. The masked man dashes\
    \ through a wide set of doors atop the stairs, and you follow close behind.\
    \ The sound of crashing thunder is muffled as the doors close behind you.\
    \ Looking up, you see a familiar depiction in stained glass. “Beautiful, is\
    \ it not?” the Stranger says quietly."
  ]

dream11 :: Message
dream11 = FlavorText
  (Just "Dream 11")
  [ "You avoid looking at the stained glass. If this vile man\
    \ considers it to be beautiful, it is probably some trap meant to ensnare\
    \ your mind. You clench your fist until your knuckles are white and step\
    \ forward to confront the Stranger"
  ]

dream12 :: Message
dream12 = FlavorText
  (Just "Dream 12")
  [ "You peer up at the stained glass, curious. What is the shape\
    \ of the shadow along the window? What is the meaning behind this\
    \ strange design?"
  ]

dream13 :: Message
dream13 = FlavorText
  (Just "Dream 13")
  [ "Daniel’s voice calls out to you. “They are opening the path to\
    \ Carcosa.”"
  ]

awakening :: Message
awakening = FlavorText
  (Just "Awakening")
  [ "You awaken from your fitful dream, sweating and\
    \ gagging. This cannot go on any longer. You have only one option if\
    \ you are to continue your investigation. You must find Nigel Engram,\
    \ director of The King in Yellow, and architect of this madness. Only he\
    \ will have the answers you seek. You pack your bags and plan your trip to\
    \ Paris, the City of Lights."
  ]

jordansInformation :: Message
jordansInformation = FlavorText
  (Just "Jordan's Information")
  [ "According to Mr. Jordan Perry, who had\
    \ financed several performances of The King in Yellow across the world,\
    \ Nigel Engram was an eccentric and impassioned man, almost to the\
    \ point of mania. Rumor was, he hadn’t directed any other works since\
    \ discovering The King in Yellow. Jordan had first met with Mr. Engram\
    \ at a café in Montparnasse, “L’agneau Perdu.” You travel there first,\
    \ hoping to find Mr. Engram..."
  ]
