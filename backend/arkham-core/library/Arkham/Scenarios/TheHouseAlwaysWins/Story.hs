module Arkham.Scenarios.TheHouseAlwaysWins.Story where

import Arkham.Prelude

import Arkham.Text

intro :: FlavorText
intro =
  FlavorText
    (Just "Scenario I-B: The House Always Wins")
    [ "Dr. Armitage suggested you track down his associate Dr. Francis Morgan.\
      \ He’s not sure whether Dr. Morgan is in trouble, but he’s not particularly\
      \ happy with his colleague’s present choice of company. He’s in the Clover Club,\
      \ a notorious gambling joint somewhere downtown. Finding the club’s exact\
      \ location isn’t easy—you have to grease a few palms just to learn which of the\
      \ Downtown restaurants operates as the club’s front. That restaurant is La\
      \ Bella Luna, a somewhat upscale Italian eatery by the theatre. You change into\
      \ your Sunday best and make your way there."
    , "In front of La Bella Luna stands a man in a pinstripe suit who sizes you up as\
      \ you approach. “Enjoy yourselves,” he says with a snake-like grin as he holds\
      \ open the restaurant’s front door."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "You flee to the end of the block and pause to\
      \ recover. Before you can catch your breath, the ground shakes\
      \ with a thunderous crash. People emerge from their homes and\
      \ storefronts to see what the ruckus is, and a crowd forms on\
      \ the street. You head to the front of the crowd and are horrified\
      \ to see the building from which you fled just minutes earlier\
      \ reduced to rubble. There is no sign of Dr. Morgan anywhere."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "“What in the world…?” Dr. Morgan finally\
      \ breaks out of his daze as you make your way to safety. You ask\
      \ him what he remembers, and he sputters and shakes his head.\
      \   “It’s all a haze,” he explains, visibly exhausted. “I was having\
      \ the run of my life! Perhaps I had one too many shots. But,\
      \ those creatures—I haven’t seen anything like that since…” He\
      \ trails off, and you can tell that his mind is racing. His eyes widen\
      \ with realization and his face pales. “I may not be in the best\
      \ shape, but I’ll help with your investigation. Whatever it takes.”"
    ]

resolution3 :: FlavorText
resolution3 =
  FlavorText
    (Just "Resolution 3")
    [ "Although you were unable to find Dr.\
      \ Morgan in the club, the man you rescued is grateful for your\
      \ help. He introduces himself as Peter Clover, the owner of\
      \ the establishment you’d just left. Despite the situation, he\
      \ maintains an air of quiet professionalism. As you make your\
      \ way towards the street, a well-polished Chrysler B-70 rolls\
      \ up to you, and a gorgeous woman with long brown hair and\
      \ narrow eyes exits. She is flanked by dangerous-looking men\
      \ who slip their hands under their suit jackets when they see\
      \ you. “Peter,” she says with a sigh of relief, “Good, you’re okay.\
      \   I heard there was trouble?” She turns and glares at you with\
      \ deadly eyes. “Who are they?”"
    , "Mr. Clover dusts off his vest, unworried. “Naomi, my dear, these\
      \ are friends of mine. They…” he clears his throat. “They escorted\
      \ me off the premises,” he explains after a short pause. “They have\
      \ earned our gratitude.” The woman crosses her arms and takes a\
      \ moment to size you up before giving you a smirk."
    , "“Very well then. I must thank you for taking care of Peter. Run\
      \ along now; we’ll handle things from here.” She nods to the\
      \ goons flanking her and they walk past you toward the club’s\
      \ rear entrance, pulling firearms out from underneath their coats.\
      \   You’re not sure what ‘handling things’ means, but you’re pretty\
      \ sure you don’t want to be here when the gunfire starts. You\
      \ thank Naomi and Peter, and head off."
    ]

resolution4 :: FlavorText
resolution4 =
  FlavorText
    (Just "Resolution 4")
    [ "You are pulled from the debris by several\
      \ firefighters, one of whom exclaims, “We’ve got a live one!”\
      \ A few of them patch you up, and the cops ask you what\
      \ happened. You’re certain they wouldn’t believe your story\
      \ about horrible monstrosities demolishing the building from\
      \ within. Unsure of what to say, you give a vague statement\
      \ about not remembering much. “We’re bringing you to\
      \ St. Mary’s,” one of the nurses says, pointing to a nearby\
      \ ambulance. Knowing now how dire the situation is, you slip\
      \ away while she is distracted by something else in the rubble…"
    ]
