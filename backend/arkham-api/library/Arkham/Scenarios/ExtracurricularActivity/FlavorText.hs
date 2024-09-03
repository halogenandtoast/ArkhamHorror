module Arkham.Scenarios.ExtracurricularActivity.FlavorText
where

import Arkham.Prelude

import Arkham.Text

intro :: FlavorText
intro =
  FlavorText
    (Just "Scenario I-A: Extracurricular Activity")
    [ "Dr. Armitage is worried his colleague, Professor Warren Rice, might be\
      \ in trouble, so he has asked for your help in finding his friend. He seems\
      \ unreasonably nervous about his colleague’s disappearance considering\
      \ Professor Rice has only been “missing” for a matter of hours…"
    ]

noResolution :: FlavorText
noResolution =
  FlavorText
    (Just "No resolution")
    [ "As you flee from the university,\
      \ you hear screaming from the northern end of the campus. An\
      \ ambulance passes you by, and you fear the worst. Hours later,\
      \ you learn that a ‘rabid dog of some sort’ found its way into\
      \ the university dormitories. The creature attacked the students\
      \ inside and many were mauled or killed in the attack."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "You find Professor Rice bound and gagged\
      \ in the closet of his office. When you free him, he informs you\
      \ that the strange men and women wandering around the\
      \ campus had been stalking him for hours. They cornered him\
      \ in his office and tied him up, although for what purpose, Rice\
      \ isn’t sure. You inform him that Dr. Armitage sent you, and\
      \ Rice looks relieved, although he suspects that Dr. Morgan\
      \ might be in danger as well. Because the strangers on campus\
      \ seem to have been targeting Professor Rice, you decide that\
      \ the best course of action is to escort him away from the\
      \ campus as quickly as possible. As you leave the university,\
      \ you hear screaming from the northern end of the campus. An\
      \ ambulance passes you by, and you fear the worst. Hours later,\
      \ you learn that a ‘rabid dog of some sort’ found its way into\
      \ the university dormitories. The creature attacked the students\
      \ inside, and many were mauled or killed in the attack."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "You pull each of the dormitory’s fire alarms\
      \ and usher the students out of the building’s north exit,\
      \ hoping to make your way off campus. Many of the students\
      \ are confused and exhausted, but you believe an attempt to\
      \ explain the situation will do more harm than good. Minutes\
      \ later, a terrible screech echoes across the campus, piercing\
      \ and shrill. You tell the students to wait and head back to the\
      \ dormitories to investigate. Oddly, you find no trace of the\
      \ strange creature—a prospect that worries you more than it\
      \ relieves you. You hurry to the faculty offices to find Professor\
      \ Rice, but there is no sign of him anywhere."
    ]

resolution3 :: FlavorText
resolution3 =
  FlavorText
    (Just "Resolution 3")
    [ "After defeating the strange and terrifying\
      \ creature from the Department of Alchemy, you rush to the\
      \ faculty offices to find Professor Rice. By the time you get to his\
      \ office, there is no sign of him anywhere."
    ]

resolution4 :: FlavorText
resolution4 =
  FlavorText
    (Just "Resolution 4")
    [ "You awaken hours later, exhausted and\
      \ injured. You’re not sure what you saw, but the sight of it filled\
      \ your mind with terror. From other survivors, you learn that\
      \ a ‘rabid dog of some sort’ found its way into the university\
      \ dormitories. The creature attacked the students inside, and\
      \ many were mauled or killed in the attack."
    ]
