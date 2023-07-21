module Arkham.Scenarios.TheEssexCountyExpress.Story where

import Arkham.Prelude

import Arkham.Text

intro :: FlavorText
intro =
  FlavorText
    (Just "Scenario III: The Essex County Express")
    [ "Recent events in the Museum have forced you to\
      \ re-evaluate Armitage’s tale about Dunwich. It\
      \ cannot be a coincidence—Wilbur Whateley, the\
      \ Necronomicon, the creature from Dunwich, and\
      \ the people and creatures who attacked here in\
      \ Arkham—everything must be connected. You’re\
      \ certain now where you must head: the lonely\
      \ and dismal town of Dunwich Village."
    , "You consider telling the Massachusetts State Police what you know, but\
      \ the negative consequences outweigh the potential gain. Given the nature\
      \ of your story, they would likely write your stories off as an absurd hoax.\
      \ Worse, they could lock you up. After all, you were present in an illegal\
      \ speakeasy, and you also trespassed in the museum. Instead, you decide\
      \ to head to Dunwich yourself, in order to investigate further."
    , "You pack everything you think you might need and manage to get\
      \ some rest for the night. In the morning, you head to the train station in\
      \ Northside and purchase a last-minute express ticket. Dunwich is several\
      \ hours by train northwest along the Miskatonic River Valley. There is no\
      \ train station in Dunwich, but you manage to phone one of Armitage’s\
      \ acquaintances in the small village: a man by the name of Zebulon\
      \ Whateley who was present during the events several months ago."
    , "Armitage’s notes indicate that the Whateley family is spread across\
      \ many branches, some decadent and unscrupulous, others “undecayed”\
      \ or otherwise untouched by nefarious and diabolic rites. According to\
      \ Armitage, Zebulon’s branch of the family lay somewhere between the\
      \ decayed and undecayed Whateleys, who knew of the traditions of his\
      \ ancestors, but was not corrupted by them. He agrees to pick you up at\
      \ the closest station and drive you into town."
    , "As the train departs from Arkham, you feel the events of the previous\
      \ night catching up to you, and exhaustion sets in. But before you can\
      \ safely reach your destination, the train car suddenly rumbles and\
      \ shakes, startling you out of your reverie. The train loudly skids to a\
      \ violent halt, and you hear a rattling noise behind you…"
    ]

investigatorDefeat :: FlavorText
investigatorDefeat =
  FlavorText
    (Just "Investigator Defeat")
    [ "Your experience beyond the gate is\
      \ simultaneously terrifying and impossible to recall with clarity.\
      \ A hypnotic spectacle of lights, otherworldly sensations, and\
      \ altered geometry dances at the tattered edges of your mind. An\
      \ unearthly voice from beyond rings in your ears, its significance\
      \ an enigma. When you awaken, you find yourself in the woods,\
      \ several miles from the Miskatonic River. Destroyed train cars\
      \ surround you. They are crumpled as if from a severe impact;\
      \ they are also decayed as if years of rust and squalor have\
      \ claimed them. There is no sign of the other passengers."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "You breathe a sigh of relief as the gate behind\
      \ the train collapses harmlessly upon itself. The few passengers\
      \ who survived the ordeal seem unable to comprehend what\
      \ just happened. One passenger mentions “a pipe bursting in\
      \ the rear car,” and that quickly becomes the explanation for\
      \ the innocent and ignorant, those who either cannot or choose\
      \ not to delve further into the mystery. You, on the other hand,\
      \ know better… although in hindsight, you wish you didn’t."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "Rattled,\
      \ you begin walking alongside the train tracks, making your\
      \ way towards Dunwich."
    ]
