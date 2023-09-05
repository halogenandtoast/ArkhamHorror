module Arkham.Scenarios.UnionAndDisillusion.Story where

import Arkham.Prelude

import Arkham.Text

intro :: FlavorText
intro =
  FlavorText
    (Just "Union and Disillusion")
    [ "The Unvisited Isle lies in the midst of the Miskatonic River, in between two of\
      \ the bridges that span the river and connect the Downtown and Rivertown\
      \ neighborhoods of Arkham. Overgrown with vines, thorns, and unnatural\
      \ underbrush, the island is home to no residents and very few fauna. Rows of\
      \ mysterious stone pillars stand sentry throughout the island, rising high above\
      \ the treetops. Though many theories abound, none know for sure what purpose\
      \ these stone pillars serve, or who built them. Very few dare to venture onto the\
      \ shores of the enigmatic island, and those who do return with strange tales:\
      \ bonfires in the woods at the darkest hours of night—stone altars with deep\
      \ carvings impossible Setup to translate—empty woods devoid of life, save for the\
      \ stark-black ravens and whippoorwills who perch on the tree branches and watch\
      \ trespassers with eager eyes. Unfortunately for you, the stakes are too high for\
      \ you to heed these many warnings."
    , "The streets of Arkham are emptier and quieter than you have ever seen them\
      \ before. A familiar grey mist has enveloped the town. The haze grows denser and\
      \ thicker the closer you get to the river. You move with haste, not wanting to\
      \ encounter any others on your way to the site of the ritual, whether they be\
      \ living or dead. You find several rowboats moored by the docks and climb aboard\
      \ one of them. With the waters unnaturally still and barely any current carrying\
      \ the boat, rowing toward the island should be a simple endeavor. Even so, the\
      \ dense fog makes it difficult for you to get your bearings. Soon, you are\
      \ completely enclosed by the grey mist, unable to see the banks of the river or\
      \ the coast of the island. Shapes emerge from the fog. Ripples appear on the\
      \ surface of the water. The gaze of the watcher is upon you."
    , "Just then, a pillar of spectral energy ascends into the sky, clear as day even\
      \ through the haze. The shapes all turn their attention to this beacon, and you\
      \ sense the watcher’s gaze slipping from your soul. The rite has begun. You row\
      \ faster, using the light to guide you. It is time to decide once and for all why\
      \ you have come—are you here to aid the Silver Twilight Lodge? Or to disrupt\
      \ them?"
    ]

noResolution :: FlavorText
noResolution =
  FlavorText
    (Just "No Resolution")
    [ "You barely remember sprinting back to shore and getting in your rowboat. It is\
      \ as though you were possessed—by a fit of madness or by a spirit bent only on\
      \ survival, you cannot say. You are shaken to your senses by the powerful gale\
      \ which sweeps across the river, almost capsizing your boat. The spectral pillar\
      \ marking the site of the Lodge’s ritual suddenly bursts outward. Dozens of\
      \ specters and phantasmal shapes fly across the treetops and glide across the\
      \ water. You will never forget their shrieks, like hundreds of deathcries\
      \ sounding at once."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "The revenant thrashes and writhes in torment, unable to escape with its\
      \ connection to the spectral realm torn away. Carl Sanford steps forward, opens a\
      \ thick leather tome, and the ghost’s memories—along with the remainder of its\
      \ form—are unceremoniously pulled into its pages."
    , "“What have you done?!” Anette yells. Several members of her coven surround her.\
      \ They are embattled, but you have no doubt they would lay down their life to\
      \ protect their high priestess from harm."
    , "“What have I done? Why, I have cheated the Devil,” Sanford responds with a\
      \ sinister grin. You notice the words of his tome are now glowing with a spectral\
      \ radiance. “Keziah signed the Black Book of Azathoth in order to learn the\
      \ secrets of the universe. A fatal error, in my estimation, for it put her in the\
      \ clutches of one far more powerful than she.” He begins to read from the pages\
      \ as he explains further. “She paid the price, but I shall reap the harvest.”"
    , "Anette clenches her jaw and turns toward you. “You cannot even begin to fathom\
      \ the secrets this monster now possesses. Do you truly believe he will use this\
      \ power for good?” She asks."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "You stand tall and declare your loyalty proudly. The other members of the Order\
      \ nod in affirmation. Carl Sanford will lead mankind to an era of peace, and\
      \ protect them from the threats of the cosmos. It may take time for society to\
      \ adjust, but humanity can only survive if these secrets are in the hands of the\
      \ Order."
    , "The witches retreat into the woods, and you are left alone with your brothers\
      \ and sisters of the Inner Circle. You gather around your leader, and as he reads\
      \ from the New Creed of the Silver Twilight Order, he informs you of his plans:\
      \ of the higher beings of the cosmos, of humanity’s ascension, of the Great Old\
      \ Ones, of life and death, of Azathoth and the End of Everything. Step by step,\
      \ the Order plots the future of the Earth, and you will become instrumental in\
      \ the new world order."
    ]

resolution3 :: FlavorText
resolution3 =
  FlavorText
    (Just "Resolution 3")
    [ "Anette is right. Sanford tricked you. This is what he was after all along.\
      \ Perhaps he considers this “protecting humanity,” but the truth is that he has\
      \ endangered the lives of others over and over again in order to serve his own\
      \ greed and lust for power. You shake your head."
    , "“A pity,” Mr. Sanford complains, “I thought you were smarter than this.” You\
      \ take several steps back, retreating to where the witches are gathered. Sanford\
      \ turns to the other members of his Order and points to you. “Kill them,” he\
      \ commands."
    , "“Erynn, give us cover!” Anette barks to the red-haired witch next to her. The\
      \ witch nods in response and waves a wooden rod in front of her, creating a cloud\
      \ of mist that masks your presence. “We have to get out of here. Go!” Anette\
      \ yells."
    , "Your escape is a blur. Robed members of the Silver Twilight cult chase you\
      \ relentlessly throughout the forsaken woods. You are only barely able to make it\
      \ back to the shore before they catch up to you. You motion for Anette and the\
      \ others to join you, but she shakes her head. “We’ll meet again soon.” The edges\
      \ of the witches’ cloaks bleed into the night sky, and with a swirl of darkness,\
      \ they vanish one by one."
    ]

resolution4 :: FlavorText
resolution4 =
  FlavorText
    (Just "Resolution 4")
    [ "Anette approaches, confronting the remnant’s ethereal shell. “Sister! We are\
      \ the ones who have called you here from the great beyond.” The shattered\
      \ revenant turns its attention to Anette, and the rest of her coven watch in\
      \ anticipation."
    , "“What are you doing, you fool? This spirit is no mere witch!” Carl Sanford\
      \ yells out. But it is too late. Anette ignores Mr. Sanford, continuing to speak\
      \ with the spectral presence. “For years we have suffered injustice after\
      \ injustice. Share with me your secrets, sister. Together, as one mind, body, and\
      \ soul, we can begin anew.”"
    , "The ghost reaches out to her with a long, skeletal arm. Anette’s sisters gasp\
      \ as their high priestess steps forward and kneels before the being, offering\
      \ herself. With a sudden rush of energy, the phantom soul is drawn inside Anette.\
      \ She collapses to the ground, reeling from the spiritual impact. Her body\
      \ writhes in torment, but none of the other witches step forward to help, instead\
      \ watching with equal parts terror and fascination. Finally, Anette rises to her\
      \ feet, her eyes aglow with mystical power. When she speaks, it is not her voice.\
      \ “Sisters. Rejoice, for I am reborn,” Keziah says."
    , "Carl Sanford taps you with his cane. You were so transfixed by the events\
      \ occurring in front of you that you hadn’t noticed the Lodge members slinking\
      \ toward the edges of the clearing. One of the Lodge’s inner circle raises a\
      \ revolver and attempts to fire, but the high priestess simply glares at him, and\
      \ his arm withers and rots before your eyes. His gun clatters to the ground and\
      \ he makes one final croak before he is nothing but a decayed husk on the ground.\
      \ “We must leave this place at once,” Sanford says stoically. “This being\
      \ possesses power over life and death itself. It will not suffer our presence any\
      \ longer.”"
    , "Anette—no, Keziah—turns her attention to you, and her eyes brighten. “You look\
      \ familiar, child. Come, come. Do not be afraid. You have nothing to fear from\
      \ me.” She grins wickedly. Her voice is like knives driving into your soul. You\
      \ hate to admit it, but Sanford is right. You turn and flee with the rest of the\
      \ Lodge, hoping you are fast enough to escape the witch’s magic."
    , "Your escape is a blur. Witches and spirits chase you relentlessly throughout\
      \ the forsaken woods. You are only barely able to make it back to the shore\
      \ before they catch up to you. Other members of the Lodge are perhaps not so\
      \ fortunate. Having scattered in many directions, you’re not sure how many of\
      \ them escaped, and you don’t see Carl Sanford anywhere, though you suspect he\
      \ had a plan for this outcome."
    ]

resolution6 :: FlavorText
resolution6 =
  FlavorText
    (Just "Resolution 6")
    [ "When you come to your senses, the wind has subsided and the dark mist has\
      \ vanished. With the ritual concluded, the island seems to have returned to\
      \ normal—but was the ritual successful, or did it fail? You make your way to the\
      \ center of the island to find out what happened. As you arrive, you find Anette\
      \ and her coven heavily wounded, and the Lodge stands victorious. Carl Sanford\
      \ holds a thick leather tome in his hands, its pages glowing with arcane power.\
      \ “What have you done?!” Anette yells. Several members of her coven surround her.\
      \ They are embattled, but you have no doubt they would lay down their life to\
      \ protect their high priestess from harm."
    , "“What have I done? Why, I have cheated the Devil,” Sanford responds with a\
      \ sinister grin. You notice the words of his tome are now glowing with a spectral\
      \ radiance. “Keziah signed the Black Book of Azathoth in order to learn the\
      \ secrets of the universe. A fatal error, in my estimation, for it put her in the\
      \ clutches of one far more powerful than she.” He begins to read from the pages\
      \ as he explains further. “She paid the price, but I shall reap the harvest.”"
    , "Anette clenches her jaw and turns toward you. “Listen to me. You cannot even\
      \ begin to fathom the secrets this monster now possesses. You must stop him, no\
      \ matter the cost.”"
    , "You hold your tongue, but in the back of your mind, you suspect Anette is\
      \ right. Sanford has tricked you. This is what he was after all along. Perhaps he\
      \ considers this “protecting humanity,” but the truth is that he has endangered\
      \ the lives of others over and over again in order to serve his own greed and\
      \ lust for power. However, before you can confront Sanford, he turns to the other\
      \ members of his Order and points to you. “This one is no longer useful,” he\
      \ explains. “Kill them all.”"
    , "“Erynn, give us cover!” Anette barks to the red-haired witch next to her. The\
      \ witch nods in response and waves a wooden rod in front of her, creating a cloud\
      \ of mist that masks your presence. “We have to get out of here. Go!” Anette\
      \ yells."
    , "Your escape is a blur. Robed members of the Silver Twilight cult chase you\
      \ relentlessly throughout the forsaken woods. You are only barely able to make it\
      \ back to the shore before they catch up to you. You motion for Anette and the\
      \ others to join you, but she shakes her head. “We’ll meet again soon.” The edges\
      \ of the witches’ cloaks bleed into the night sky, and with a swirl of darkness,\
      \ they vanish one by one."
    ]

resolution7 :: FlavorText
resolution7 =
  FlavorText
    (Just "Resolution 7")
    [ "When you come to your senses, the wind has subsided and the dark mist has\
      \ vanished. With the ritual concluded, the island seems to have returned to\
      \ normal—but was the ritual successful, or did it fail? You make your way to the\
      \ center of the island to find out what happened. As you arrive, you find Carl\
      \ Sanford and the other members of his Order heavily wounded, and Anette and her\
      \ coven stand victorious. Anette stands at the center of the clearing, her eyes\
      \ aglow with mystical power. When she speaks, it is not her voice. “Sisters.\
      \ Rejoice, for I am reborn,” Keziah says."
    , "Carl Sanford taps you with his cane. You were so transfixed by the events\
      \ occurring in front of you that you hadn’t noticed the Lodge members slinking\
      \ toward the edges of the clearing. One of the Lodge’s inner circle raises a\
      \ revolver and attempts to fire, but the high priestess simply glares at him, and\
      \ his arm withers and rots before your eyes. His gun clatters to the ground and\
      \ he makes one final croak before he is nothing but a decayed husk on the ground.\
      \ “We must leave this place at once,” Sanford says stoically. “This being\
      \ possesses power over life and death itself. It will not suffer our presence any\
      \ longer.”"
    , "Anette—no, Keziah—turns her attention to you, and her eyes brighten. “You look\
      \ familiar, child. Come, come. Do not be afraid. You have nothing to fear from\
      \ me.” She grins wickedly. Her voice is like knives driving into your soul. You\
      \ hate to admit it, but Sanford is right. You turn and flee with the rest of the\
      \ Lodge, hoping you are fast enough to escape the witch’s magic."
    , "Your escape is a blur. Witches and spirits chase you relentlessly throughout\
      \ the forsaken woods. You are only barely able to make it back to the shore\
      \ before they catch up to you. Other members of the Lodge are perhaps not so\
      \ fortunate. Having scattered in many directions, you’re not sure how many of\
      \ them escaped, and you don’t see Carl Sanford anywhere, though you suspect he\
      \ had a plan for this outcome."
    ]

resolution8 :: FlavorText
resolution8 =
  FlavorText
    (Just "Resolution 8")
    [ "When you finally return to the banks of the Miskatonic River, you take stock of\
      \ the situation..."
    ]
