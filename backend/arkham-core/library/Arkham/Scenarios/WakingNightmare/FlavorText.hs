module Arkham.Scenarios.WakingNightmare.FlavorText where

import Arkham.Prelude

import Arkham.Text

intro1 :: FlavorText
intro1 =
  FlavorText
    (Just "Intro 1")
    [ "It has been more than twenty-four hours since your companions fell\
      \ asleep. Over the course of the past day, your curiosity has slowly turned\
      \ to concern and then dread. The problems began when one of your friends\
      \ started to toss and turn violently in their sleep. You snapped to attention\
      \ and tried to shake them awake, but it was to no avail. You tried\
      \ everything. Physical contact was no use, and water did nothing but soak\
      \ their clothes and bed fruitlessly. Even opening their eyelids did not wake\
      \ them, and in the process, you noticed that their pupils had fully dilated\
      \ and their eyes were glazed over with a milky-white fog."
    , "You have no idea what this could mean for your friends. Have they\
      \ managed to find their way to the land Virgil Gray described in his\
      \ writings? Or has something more sinister taken root within their minds and\
      \ bodies? Just to be safe, you decide to take your companions to St. Mary’s\
      \ Hospital. If something ails them physically, perhaps the doctors there can\
      \ discern what is wrong. Otherwise, you will simply have to watch over them\
      \ and hope that they return safely to the waking world."
    , "St. Mary’s is the only hospital in the town of Arkham, and it is a\
      \ fixture of its Uptown neighborhood. You explain the situation to Nurse\
      \ Greenberg, the head nurse at the hospital, who instructs several other\
      \ nurses to carry your companions into the emergency ward on stretchers. She\
      \ tells you with a warm, friendly smile that your friends will be examined by\
      \ Doctor Maheswaran, but other than that, you are left entirely in the dark."
    , "Hours pass. You hear nothing about the status of your companions. You\
      \ still have not met with Doctor Maheswaran, and you’re starting to grow\
      \ impatient. You feel something crawling along your arm, and you\
      \ instinctively brush it away, then wonder if it was an insect or a figment\
      \ of your imagination. You begin to question whether bringing your friends\
      \ here was the right choice after all. Eventually, you decide to take matters\
      \ into your own hands."
    , "It is late at night, and the receptionist who instructed you to stay in\
      \ the waiting room is nowhere to be seen. In fact, there are eerily few\
      \ people roaming the halls of the hospital. With nobody to stop you, you\
      \ sneak off into the emergency ward to find your friends. It does not take\
      \ long for you to find their room. Your companions lie asleep on clean, white\
      \ cots, their sleep anything but peaceful. They are pale faced and sweaty.\
      \ One of them tosses and turns in their sleep, their brow furrowed with pain\
      \ or worry."
    , "Doctor Maheswaran does not seem surprised by your intrusion. “Shivani\
      \ Maheswaran,” she introduces herself coldly, without looking up from her\
      \ clipboard. “You’re the ones who brought them in, right? Before you ask: no,\
      \ I’ve never seen anything like this before in my life,” she says with a hint\
      \ of dry impatience. You demand that she tell you everything she knows about\
      \ their condition and not give you the runaround. With a sigh, she puts her\
      \ clipboard down and addresses you frankly. “Listen, I may be new to Arkham,\
      \ but I have seen enough inexplicable maladies in this town to fill entire\
      \ careers in medicine. I understand why you are concerned. Your friends...”\
      \ She struggles to find the right words. “They are not simply asleep. It is\
      \ as if they are hovering somewhere between sleep and unconsciousness, or\
      \ even death. They are not aware of anything happening around them but appear\
      \ to be reacting to some kind of internal stimuli.”"
    , " Without hesitation, you ask if they are dreaming. “Dreaming?” Doctor\
      \ Maheswaran replies. “It is unlikely, though it might account for their\
      \ mannerisms. Honestly—and I know this is not exactly reassuring— none of\
      \ this makes any sense to me, medically speaking.”"
    , "Just then, you see a large, hairy spider crawling on the chest of one of\
      \ your friends. “Well, that is odd.” Doctor Maheswaran brushes the spider\
      \ off, and several more emerge from the sheets to take its place. You and the\
      \ doctor both take several steps back out of pure instinct. You hear heavy\
      \ footsteps in the hallway outside the emergency ward, and then the lights\
      \ begin to flicker. “Okay...that is more than simply odd. What in the world\
      \ is going on out there?” Doctor Maheswaran asks anxiously."
    , "For just a moment, you think you hear one of your sleeping companions\
      \ whisper something. Are they indeed dreaming? And if so, what does their\
      \ condition have to do with these strange events?"
    ]

intro2 :: FlavorText
intro2 =
  FlavorText
    (Just "Intro 2")
    [ "“Yes, I suppose that makes sense. There has been no change in their\
      \ condition for the past few hours, anyway.” Doctor Maheswaran’s gaze shifts\
      \ nervously to and fro, searching for more spiders. “Also, I’d very much like\
      \ to get out of this room now, so...lead the way,” she adds. You nod and\
      \ venture back into the hospital’s waiting room."
    ]

intro3 :: FlavorText
intro3 =
  FlavorText
    (Just "Intro 3")
    [ "“Yes, of course. Their safety is paramount. But do come back and tell me\
      \ what is going on, please.” She shivers. “I really hate this place after\
      \ dark...” You nod and venture back into the hospital’s waiting room."
    ]

noResolution :: FlavorText
noResolution =
  FlavorText
    (Just "No Resolution")
    [ "With panic overtaking your mind, you run through the main exit of St.\
      \ Mary’s Hospital, putting as much distance as you can between you and this\
      \ accursed infestation. The moment you get home, you collapse from exhaustion."
    , "The next day, you wake and check the morning papers, expecting to see\
      \ tales of chaos at St. Mary’s and of an infestation of arachnids. However,\
      \ there is no news regarding the hospital at all. Did last night’s events occur\
      \ too late for the Arkham Advertiser to report on them? Was it all just a bad\
      \ dream? You put on your coat and head back to the hospital to confirm that\
      \ what you saw last night was real."
    , "When you arrive at St. Mary’s, you expect to see the building—or perhaps\
      \ all of Uptown—covered in spiderwebs. But instead, to your surprise, it is\
      \ perfectly clean. Patients, nurses, and doctors walk through its halls like\
      \ none of last night’s events transpired. Before you can find your sleeping\
      \ companions, however, a blond-haired man with heavy bags under his eyes\
      \ approaches you and places a hand on your shoulder."
    , "“Good morning. We should talk.”"
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "Before you depart from the hospital, Doctor Maheswaran announces that\
      \ she is going to check on her patients, and you decide to join her. You head\
      \ back to the emergency ward and find that your companions are still\
      \ unconscious. Doctor Maheswaran checks their eyes, takes their pulses, and\
      \ shakes her head. “It’s even worse than before. Whatever internal stimuli\
      \ they are reacting to, they are causing the patients’ minds and bodies a\
      \ great deal of stress.” She sits down next to one of her patients and shakes\
      \ her head. “I’ll stay here. You go talk to that patient and get to the\
      \ bottom of this, okay? Oh, and if you find any more spiders, squash one for\
      \ me, please.” As you leave, you swear to your sleeping friends that you will\
      \ get to the bottom of this and save them."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "Before you depart from the hospital, you search high and low for Doctor\
      \ Maheswaran, but she is nowhere to be found. You head back to the emergency\
      \ ward to check on your companions and find that they are still unconscious.\
      \ In fact, their condition seems to have grown worse. Their foreheads are\
      \ covered in sweat, and blood drips from their closed eyes. One of them is\
      \ whispering something in their sleep, a repeated phrase in a language you\
      \ cannot understand. As you leave, you swear to your sleeping friends that\
      \ you will get to the bottom of this and save them."
    ]

resolution3 :: FlavorText
resolution3 =
  FlavorText
    (Just "Resolution 3")
    [ "Before you depart from the hospital, you go back to the emergency ward\
      \ to check on Doctor Maheswaran. The door to the room where your companions\
      \ are staying is sealed shut, so you knock on the door and ask if she is\
      \ there. On the other side, you hear furniture being shifted aside and\
      \ knocked over. An exhausted Shivani Maheswaran cracks open the door and\
      \ peers at you with bloodshot eyes. “Oh, thank goodness it is you.” She lets\
      \ you in and collapses onto a nearby chair. “Those...things tried to get into\
      \ the room. I had to barricade myself inside to stop them.” You inform her\
      \ that the danger has passed and thank her for watching over your friends.\
      \ “I’ll continue to keep an eye on them,” she says. “Just please, tell me\
      \ there are no more spiders out there, or I’m taking the first train out of\
      \ this town.” As you leave, you swear to your sleeping friends that you will\
      \ get to the bottom of this and save them."
    ]

resolution4 :: FlavorText
resolution4 =
  FlavorText
    (Just "Resolution 4")
    [ "With panic overtaking your mind and no idea how you can escape, you\
      \ flee anywhere your feet will take you. Hideous monsters begin to close in\
      \ around you, and you envision yourself wrapped in webs, waiting to be a\
      \ spider’s meal. Eventually you come to a dead end: a room covered from floor\
      \ to ceiling in thick, sticky webs. The skittering of spider legs follows\
      \ close behind, and you realize you have no hope to escape unless there is a\
      \ path behind the webs. There is no choice remaining for you. You run\
      \ headlong into the webs, ripping and tearing at them with the ferocity of a\
      \ cornered animal. It is tough work, but eventually you see a light behind\
      \ the webs—perhaps an escape route."
    , " Once you cross through the webs, you are no longer in the dark, sterile\
      \ halls of St. Mary’s, but in a narrow, web-covered cavern. You dare not\
      \ tarry or go back the way you came, so you run through the dark, cramped\
      \ cave with no idea where it might lead. Glancing through the cobwebs to your\
      \ left and right, you see not just hard rock, but the glimmering of lights,\
      \ like a sea of stars looming beyond the silken threads. Eventually, you\
      \ emerge from another wall of webs, only to find yourself in an alleyway in\
      \ Arkham’s Merchant District, near the Miskatonic River. You have neither the\
      \ time nor the desire to analyze this, instead deciding to put as much\
      \ distance as you can between you and the hospital. The moment you get home,\
      \ you collapse from exhaustion."
    , "The next day, you wake and check the morning papers, expecting to see\
      \ tales of chaos at St. Mary’s and of an infestation of arachnids. However,\
      \ there is no news regarding the hospital at all. Did last night’s events\
      \ occur too late for the Arkham Advertiser to report on them? Was it all just\
      \ a bad dream? You put on your coat and head back to the hospital to confirm\
      \ that what you saw last night was real."
    , "When you arrive at St. Mary’s, you expect to see the building—or perhaps\
      \ all of Uptown—covered in spiderwebs. Instead, to your surprise, it is\
      \ perfectly clean. Patients, nurses, and doctors walk through its halls like\
      \ none of last night’s events transpired. You ask the receptionist if you can\
      \ see Doctor Maheswaran, but he shakes his head. “I’m sorry, but I don’t\
      \ think she’s here today. She left in the middle of her shift last night, and\
      \ nobody has seen her since. Is there somebody else I can reach for you?”"
    , "Something is not right. None of this makes any sense. You are about to\
      \ demand to see your sleeping companions when a blond-haired man with heavy\
      \ bags under his eyes approaches you and places a hand on your shoulder."
    , "“Good morning. We should talk.”"
    ]

resolution5 :: FlavorText
resolution5 =
  FlavorText
    (Just "Resolution 5")
    [ "You depart from the hospital with the blond-haired man in tow, and he\
      \ introduces himself more fully. The man, whose name is Randolph Carter,\
      \ tells you that he is a “dreamer,” like Virgil Gray and your friends: one\
      \ with the ability to traverse the divide between the waking world and a\
      \ parallel dimension that was created by, is sustained by, and dwells within\
      \ the dreams of all living organisms on Earth—a realm he calls The\
      \ Dreamlands. “It is a place of both dreams and nightmares,” he explains.\
      \ “And I’m afraid your friends are trapped there as we speak.”"
    , "You ask if there is anything you can do to aid them. Randolph ponders\
      \ this for a moment, then replies: “Normally, dreamers can return to the real\
      \ world simply by willing themselves awake, but for some reason, your friends\
      \ are unable to do so. If you truly wish to help them, you too must enter the\
      \ Dreamlands and find them. But forcing your way into the Dreamlands is not\
      \ easy. Such paths are usually accessible only to experienced dreamers, and\
      \ only in sleep. Still, there are some places where the Dreamlands touch the\
      \ waking world, and in those places, with the right tools, you may enter the\
      \ Dreamlands in your physical body. I know of such a place, not far from\
      \ here. However—”"
    , "Before Randolph is able to finish his sentence, he is interrupted by a\
      \ mysterious voice..."
    ]
