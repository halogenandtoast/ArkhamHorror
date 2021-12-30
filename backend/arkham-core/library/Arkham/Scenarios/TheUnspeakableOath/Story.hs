module Arkham.Scenarios.TheUnspeakableOath.Story where

import Arkham.Prelude

import Arkham.Message

intro1 :: Message
intro1 = FlavorText
  (Just "Scenario IV: The Unspeakable Oath")
  [ "“Wake up, Daniel.” The words\
    \ echo in your thoughts as you pace through the\
    \ disheveled room, trying to make sense of what\
    \ you’d just experienced. You had awoken that\
    \ afternoon with a violent startle, your body shaking, bedsheets soaked\
    \ with sweat. Although you’d slept a healthy eight-and-a-half hours, you\
    \ feel even more exhausted than you had the previous night. You catch a\
    \ glimpse of your dark, bloodshot eyes in the mirror next to the dresser,\
    \ and wonder if you’ve gotten any real sleep at all."
  , "Who is Daniel, you wonder? The chamber from your dream looked\
    \ like a prison, but the man—Daniel—he seemed unhinged, as though\
    \ he wasn’t in control of his mind. This led to only one conclusion: An\
    \ asylum. Knowing you’ll get little rest here, you head towards Arkham\
    \ Asylum, hoping your hunch is correct."
  ]

intro2 :: Message
intro2 = FlavorText
  (Just "Scenario IV: The Unspeakable Oath")
  [ "Over the course of the next few days, you delve into the\
    \ evidence you’ve collected, hoping to find any information regarding\
    \ Daniel Chesterfield, a stagehand during the previous production of\
    \ The King in Yellow. As far as you can tell, he is the only surviving\
    \ member of that production’s cast and crew. The rest of them—that is,\
    \ those for whom you can find any records at all—disappeared or died\
    \ soon after opening night in a variety of fashions connected only by their\
    \ morbidness. Freak accidents. Suicides. Vanishings."
  , "It would seem that Daniel is your only lead, if you are to investigate\
    \ further. According to the records you found, he was admitted to\
    \ Arkham Asylum many years ago. All documentation about Daniel’s\
    \ treatment seems to end there. You’re unsure if he’s even still alive.\
    \ Perhaps he was cured and released. You were hoping to avoid this, but\
    \ there seems to be only one way to find out. You collect your belongings\
    \ and head downtown, towards Arkham Asylum."
  ]

intro3 :: Message
intro3 = FlavorText
  Nothing
  [ "As you enter the asylum, you stop to speak with the\
    \ receptionist, though you feel your body urged to step deeper into the\
    \ clutches of this madhouse. He gives you a confused expression as you\
    \ tell him of The King in Yellow and of Daniel. But at your insistence,\
    \ he pores through his file cabinet, eventually pulling out a stark white\
    \ folder. Inside is a wealth of information about the patients admitted\
    \ to the asylum—medical records, psychiatric evaluations, and the like.\
    \ You recognize a few of the faces as he flips through the pages. “Daniel…\
    \ Daniel Chesterfield, yes? He is admitted under the special care of\
    \ Doctor Mintz. But you can’t see him; his level is restricted to staff only.”\
    \ You argue and insist to be let into the patient wing, knowing that Daniel\
    \ must hold the key to understanding what is really going on."
  , "The receptionist gives a pitying smile and relents, nodding to the\
    \ security guards nearby. “Oh, of course, of course,” he says with all\
    \ the honesty of a street peddler. “I will schedule a meeting for you\
    \ with Doctor Mintz so you can speak with him about Daniel. These\
    \ gentlemen will see you in.” Relieved that you will soon get the answers\
    \ you seek, you are escorted into the patient wing of Arkham Asylum…"
  ]

constancesInformation :: Message
constancesInformation = FlavorText
  (Just "Constances's Information")
  [ "You recall what Constance had told you\
    \ when you spoke with her during her hellish dinner party. She and the\
    \ other members of the cast and crew had been told by the director, Nigel\
    \ Engram, to take some kind of oath. At first, she’d written it off as the\
    \ whim of an eccentric artist; something Mr. Engram did as a strange\
    \ formality to unite and strengthen the bonds of the cast and crew. And\
    \ to his credit, she claimed that it had worked; ever since they’d taken his\
    \ strange oath, she and the other members of the troupe felt much more\
    \ confident and full of spirit. Perhaps Daniel had a similar experience\
    \ during the last production of The King in Yellow. You must speak with\
    \ him about this."
  ]

resolution1 :: Message
resolution1 = FlavorText
  (Just "Resolution 1")
  ["…And so ended the madness of The King in\
    \ Yellow."]

resolution2 :: Message
resolution2 = FlavorText
  (Just "Resolution 2")
  [ "You are confronted by a number\
    \ of orderlies and security guards, who bar your escape. Feeling\
    \ cornered and trapped, you have no choice but to fight your\
    \ way through them. As you struggle to escape, several of the\
    \ patients nearby see their opportunity, and attack the guards\
    \ with a crazed fury. Nurse Heather shrieks and retreats, and\
    \ in the ruckus you are able to escape, bruised and battered but\
    \ free once more. It will take some time for your wounds to heal,\
    \ but you have no time for that yet."
  ]

resolution3 :: Message
resolution3 = FlavorText
  (Just "Resolution 3")
  [ "With the asylum staff distracted\
    \ and patients running amok, you are able to slip away without\
    \ being noticed. You escape deeper into the garden behind the\
    \ asylum, where a two-story tall fence topped with barbed wire\
    \ is all that separates you from the outside world. You have little\
    \ time, and need to make it far away from the asylum before the\
    \ guards return and spot you. Using a straitjacket you’d found\
    \ inside to cover the barbed wire, you scale the fence quickly,\
    \ breaking into a run as you make it to the other side."
  ]

defeat :: Message
defeat = FlavorText
  (Just "Investigator Defeat")
  [ "“Doctor Mintz, the patient has been\
    \ medicated and is ready to see you now.” Nurse Heather\
    \ opened the file on the desk nearby, taking a moment to review\
    \ the patient’s records."
  , "“Any changes in the patient’s condition?” Doctor Mintz asked\
    \ in a monotone voice. A formality. Both knew the patient was\
    \ beyond repair."
  , "“None. The patient’s delusions remain. Demanding to speak\
    \ with the Warden, claiming they are only guests, and rambling\
    \ about that play that aired the other night, The King in Yellow.”"
  , "“And their other crimes? The incident in the theatre? What\
    \ about 1452 Atlantic Ave?”"
  , "“They believe they were attacked, and were defending\
    \ themselves.” Her voice was thick with pity. What could\
    \ have caused somebody to snap like this so quickly? None\
    \ of the patient’s records showed any prior history of mental\
    \ instability. “Can you make any sense of it, Doctor?”"
  , "“Who could?” Doctor Mintz sighed and stood, grabbing the\
    \ file from his desk. “My job is not to understand. It is to cure.\
    \ Perhaps my experimental procedures will prove fruitful\
    \ on this one.” The nurse winced and felt goosebumps on her\
    \ arm. She knew how his procedures would turn out, and she\
    \ doubted it would prove anything."
  ]
