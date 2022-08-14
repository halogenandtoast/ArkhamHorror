module Arkham.Scenarios.BloodOnTheAltar.Story where

import Arkham.Prelude

import Arkham.Message

intro :: FlavorText
intro = FlavorText
  (Just "Scenario IV: Blood on the Altar")
  [ "When you finally reach Dunwich, you are\
    \ greeted by Zebulon Whateley and Earl Sawyer,\
    \ another man from the village who had met with\
    \ Dr. Armitage during the incident several months\
    \ ago. “Things ain’t lookin’ too good here,” Earl\
    \ tells you. “Some folk up and went missin’ a few\
    \ nights ago. ‘Dem whippoorwills won’ shut up.\
    \ Dunno what yer doin’ here, but last time you Arkham folk came ‘round\
    \ it was bad news. Very bad news.” His eyes blink rapidly, and he coughs\
    \ and looks away."
  , "“Look, why don’t you rest fer the night an’ look fer whatever it is yer\
    \ looking fer t’morra,” Zebulon chimes in, putting a wrinkled hand on\
    \ your shoulder. You begin to protest, but your aching muscles and weary\
    \ mind won’t allow you to refuse. The elderly man offers to take you in\
    \ for the night, and drives you to his home at the outskirts of Dunwich\
    \ village. The town is disheveled and eerie, and you find yourself wishing\
    \ you hadn’t come here at all. You fall asleep on the ride over and scarcely\
    \ remember anything else from that night."
  , "When you awaken, you find that Zebulon’s house is abandoned, and\
    \ there is no sign of the elderly man, or of Mr. Sawyer. Fearing the worst,\
    \ you head into the village of Dunwich to investigate, hoping to find\
    \ answers."
  ]

noResolution :: FlavorText
noResolution = FlavorText
  Nothing
  [ "The cries of the whippoorwills\
    \ fade into the distance, and the town of Dunwich is filled with\
    \ an eerie silence. All that can be heard is the dry whistle of the\
    \ chill wind and the slow rustling of dead leaves. There is no sign\
    \ of the missing townsfolk, nor will there be ever again."
  ]

resolution1 :: FlavorText
resolution1 = FlavorText
  (Just "Resolution 1")
  [ "As you land the finishing blow, the creature’s\
    \ body explodes into hundreds of squirming ropelike\
    \ appendages, wriggling across the ground and climbing up the\
    \ walls. You’re so startled that you aren’t fast enough to prevent\
    \ them from escaping the room. Even so, whatever that creature\
    \ was, you’re glad it’s now dead."
  ]

resolution2 :: FlavorText
resolution2 = FlavorText
  (Just "Resolution 2")
  [ "With the creature that once was Silas\
    \ lashing out at you from its chains, you have little time to\
    \ react. Knowing that the Necronomicon might have a spell\
    \ or incantation that could subdue Silas, you fend off the\
    \ abomination long enough to find a passage that can help.\
    \ With no time to spare, you recite the Latin incantation,\
    \ and find that the words come effortlessly to your tongue, as\
    \ though recalled from an earlier memory. The creature’s body\
    \ begins to shrink and melt away as the incantation builds, its\
    \ cries terrifying and haunting. In the end, all that is left is the\
    \ disfigured corpse of a man—Silas Bishop. You find a silver\
    \ pendant emblazoned with an odd constellation tucked into\
    \ his shirt. You take it with you, hoping to find a use for it."
  ]

resolution3 :: FlavorText
resolution3 = FlavorText
  (Just "Resolution 3")
  [ "With the creature that once was Silas lashing\
    \ out at you from its chains, you have little time to react.\
    \ Hoping there is something in the chamber you can use to your\
    \ advantage, you fend off the abomination long enough to find a\
    \ journal; many of its passages are written in Latin. It appears\
    \ to be a handwritten excerpt from the Necronomicon, its\
    \ purpose unknown. With no time to spare, you recite the\
    \ incantation, stumbling over the words and feeling your throat\
    \ tighten with each sentence. The creature’s body begins to\
    \ shrink and melt away as the incantation continues, its cries\
    \ terrifying and haunting. In the end, all that is left is a pile of\
    \ wet and sticky ichor, and a rotten stench."
  ]
