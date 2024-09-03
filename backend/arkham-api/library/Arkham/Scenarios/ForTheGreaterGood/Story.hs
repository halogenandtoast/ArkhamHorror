module Arkham.Scenarios.ForTheGreaterGood.Story
where

import Arkham.Prelude

import Arkham.Text

intro1 :: FlavorText
intro1 =
  FlavorText
    (Just "Scenario V: For the Greater Good")
    [ "Ever since your brush with death at Hangman’s Hill, the situation in Arkham has\
      \ only worsened. In the days since, there have been more reports of “ghost\
      \ sightings” and even several more disappearances. A thin layer of grey mist\
      \ lingers over the streets at all hours, day and night. As soon as the sun begins\
      \ to set each evening, doors are shut and locked. Without truly realizing why,\
      \ very few people dare venture out at night. Some businesses have even closed\
      \ their doors to customers during the day, citing “poor weather.” Things are\
      \ getting out of hand."
    ]

intro2 :: FlavorText
intro2 =
  FlavorText
    (Just "Intro 2")
    [ "The creature that appeared at the Lodge’s charity gala is clearly not bound to\
      \ Josef Meiger’s manor, which means the horrors you’ve experienced could be just\
      \ the start of something much worse. It is time to report your findings to the\
      \ Silver Twilight Lodge. Perhaps they can help interpret the knowledge you’ve\
      \ gathered, and inform you as to what your next step should be. With their help,\
      \ you may be able to save Arkham..."
    ]

decievingTheLodge :: FlavorText
decievingTheLodge =
  FlavorText
    Nothing
    [ "...though you suspect the Lodge has other interests. The Lodge clearly knows\
      \ more about what is happening than they care to admit. There is a sinister\
      \ purpose that lurks beneath the surface of the Lodge, and you intend to discover\
      \ what that purpose is."
    ]

intro3 :: FlavorText
intro3 =
  FlavorText
    (Just "Intro 3")
    [ "The creature that appeared at the Lodge’s charity gala is clearly not bound to\
      \ Josef Meiger’s manor, which means the horrors you’ve experienced could be just\
      \ the start of something much worse. You’re not sure why just yet, but you know\
      \ the Silver Twilight Lodge is connected to the creature in some way. Perhaps\
      \ it’s time to pay the Lodge a visit—with or without their permission."
    ]

intro4 :: FlavorText
intro4 =
  FlavorText
    (Just "Intro 4")
    [ "You can’t help but wonder if you missed something important in the home of\
      \ Josef Meiger. The four disappearances that occurred at the Lodge’s charity gala\
      \ could not have been a coincidence. If those victims encountered a creature like\
      \ the one you saw at the graveyard, the horrors you’ve experienced could be just\
      \ the start of something much worse. You’re not sure why just yet, but you are\
      \ sure that the Silver Twilight Lodge is connected to these events in some way.\
      \ Perhaps it’s time to pay the Lodge a visit—with or without their permission."
    ]

intro5 :: FlavorText
intro5 =
  FlavorText
    (Just "Intro 5")
    [ "Too many disappearances have occurred at Josef Meiger’s estate for it to be a\
      \ coincidence: first, the four victims at the charity gala, then the ones who\
      \ attended the benefit dinner one week later. If they encountered a creature like\
      \ the one you saw at the graveyard, the horrors you’ve experienced could be just\
      \ the start of something much worse. You’re not sure why just yet, but you are\
      \ sure that the Silver Twilight Lodge is connected to these events in some way.\
      \ Perhaps it’s time to pay the Lodge a visit—with or without their permission."
    ]

noResolution :: FlavorText
noResolution =
  FlavorText
    (Just "No Resolution")
    [ "You manage to escape the building and flee on foot. As you run, you can hear a\
      \ thunderous rumble coming from the manor. Several others burst through the front\
      \ door behind you, too preoccupied to notice you. One yells to her companions:\
      \ “Get out and gather at the Unvisited Isle! We have to perform the binding rite\
      \ with or without the device!”"
    , "“What about the guardian?” A man at the gates asks."
    , "“It’s no use,” she replies. “Just go!”"
    , "That is when the screaming begins. A spray of blood splatters against the\
      \ basement window. Those who escaped immediately panic and flee."
    , "You don’t stop or look back. You run until the building has faded behind the\
      \ dense, grey mist, until you can no longer hear the screams of those still\
      \ inside, until your legs can barely move."
    , "Is this the kind of “sacrifice” the Silver Twilight Lodge believes in making?\
      \ And if so, what does this mean for the “binding rite” they seek to complete at\
      \ the Unvisited Isle?"
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "Every fiber of your being is stretched as the box pulls you in. However, before\
      \ it can accomplish its task, the lid is suddenly slammed shut by an elderly\
      \ hand. You reel backward and collapse as the pulling force ceases. When your\
      \ senses finally return to you, Carl Sanford is standing over you, unflappable as\
      \ ever. “I see you managed to open the device without defeating its guardian. How\
      \ fortunate,” he declares. He examines the box closely, removing the key and the\
      \ coin from the container and recognizing them instantly. “I had a feeling you\
      \ would be a valuable asset to the Lodge, but it seems I underestimated your\
      \ resourcefulness. Perhaps it is time that you learn the truth behind our\
      \ organization. I have a feeling you are destined for great things.”\
      \ Unexpectedly, Mr. Sanford hands the puzzle box back to you, along with the\
      \ components that unlocked it. “Come with me. There is much to discuss.”"
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "Every fiber of your being is stretched as the box pulls you in. However, before\
      \ it can accomplish its task, you manage to reach out and slam the lid shut. You\
      \ shudder uncontrollably as your senses slowly return to you. This box is more\
      \ dangerous than you could have imagined. In the hands of the Lodge, it could\
      \ have been a powerful weapon, though you’re not sure what they intended to do\
      \ with it. Just then, you hear several Lodge members approaching. You quickly\
      \ shift one of the room’s bookcases aside and hide behind it before they enter\
      \ the room. “You’re sure you heard something in here? I don’t see anybody,” one\
      \ of them says."
    , "“Hm...” the other pauses, presumably inspecting the room for signs of\
      \ intruders. “Must have been nothing.”"
    , "“It’s all right. We’re all a bit on edge since the trap box went missing. Come,\
      \ we don’t want to be late for the ceremony. Tonight is an important night.”\
      \ Their footfalls become softer as they depart. The man’s words linger in your\
      \ thoughts momentarily. What did he mean by tonight being an important night?\
      \ This could be just as important as the puzzle box they had been attempting to\
      \ open. You quietly follow the two men as they head deep into the tunnels beneath\
      \ the Lodge. You struggle to keep up with them while trying to keep your\
      \ footsteps silent on the stone floor of the passageway. Finally, they enter a\
      \ large door with the familiar three arrows of Silver Twilight emblazoned along\
      \ its surface. Ritualistic chanting fills the hall from the other side. You\
      \ approach softly, placing your ear against the door to listen in."
    , "“Brothers and sisters,” an elderly voice announces, “thank you for coming to\
      \ this hallowed gathering. The time we have long waited for approaches.” You\
      \ recognize the man’s voice – it is Carl Sanford, president of the Silver\
      \ Twilight Lodge. “For many decades, the Order of the Silver Twilight has pursued\
      \ knowledge that can elevate humanity. We have defended against threats to our\
      \ very existence. We have sacrificed everything for this sacred cause. Now, one\
      \ of these threats terrorizes our city, and once again we must do what has to be\
      \ done in order to protect it.” The crowd responds with solemn approval.\
      \ “Tonight, we will complete the ritual we began many nights ago at the center of\
      \ the Unvisited Isle. Tonight, we will finally bind the revenant and learn what\
      \ it knows. We must not allow the secrets of AZATHOTH to be lost to those who\
      \ would do humanity harm.”"
    , "The cult replies with a monotone hymn, and you take this opportunity to flee\
      \ the Lodge before you are discovered. The conspiracy you have unveiled leaves\
      \ you with even more questions. If the revenant Mr. Sanford speaks of is what you\
      \ think it is, binding it will surely protect Arkham, as he claims. But what\
      \ knowledge do they seek to learn from it?...and what in the hell is an\
      \ “AZATHOTH?”"
    ]

resolution3 :: FlavorText
resolution3 =
  FlavorText
    (Just "Resolution 3")
    [ "The beast that had emerged from the box was a vicious guardian, slaughtering\
      \ anyone in its path. Somehow you were able to slay the creature, but the\
      \ consequences were dire. Many Lodge members were gruesomely killed by the\
      \ creature’s rampage before you were able to put it down. Worse, the device it\
      \ emerged from was destroyed beyond recognition. You’re not sure whether it broke\
      \ as a result of the beast’s emergence or as a result of its death, but either\
      \ way, it is a regretful outcome. “This didn’t work as planned,” one of the robed\
      \ members of the Order states matter-of-factly as you examine the crushed puzzle\
      \ box."
    , "“That’s a hell of an understatement,” another adds. She places a hand on your\
      \ shoulder and gently pulls you away from the remains of the device. “Come on,\
      \ that’s a lost cause,” she says."
    , "Her partner ignores your presence and stoically addresses her. “We’ll have to\
      \ get to the Unvisited Isle and complete the binding rite without the trap.” She\
      \ nods and takes one last look at you before the remaining Lodge members flee the\
      \ building. Is this the kind of “sacrifice” the Silver Twilight Lodge believes in\
      \ making? And if so, what does this mean for the “binding rite” they seek to\
      \ complete at the Unvisited Isle?"
    ]
