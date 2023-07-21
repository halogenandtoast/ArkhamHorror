module Arkham.Scenarios.UndimensionedAndUnseen.Story where

import Arkham.Prelude

import Arkham.Text

intro :: FlavorText
intro =
  FlavorText
    (Just "Scenario V: Undimensioned and Unseen")
    [ "Your search of the village of Dunwich has\
      \ uncovered a number of documents, journal entries,\
      \ and esoteric theories. Reading through these\
      \ materials leaves you exhausted and emotionally\
      \ drained. Most of the content was written by\
      \ a single source—a man named Seth Bishop.\
      \ When you ask around town, you learn that Seth\
      \ is a citizen of Dunwich. Along with several others, Seth had witnessed\
      \ firsthand the devastation wrought by the events of “the Dunwich\
      \ horror,” as Armitage had dubbed the incident. Curiously, since that time,\
      \ very few people had seen Seth around town, and those who did claimed\
      \ his eyes had been bloodshot and his face sweaty and pale."
    , "You don’t doubt that somebody who has seen what Seth has seen would\
      \ appear nervous or paranoid. But the more you read of his frantic and\
      \ unhinged writings, the more you believe he is involved in recent events.\
      \ His writings speak of having “gathered the remains” and of using\
      \ arcane methods to “imbue the fathers’ essence” into other creatures, and\
      \ eventually, into other people. The explanations and diagrams that follow\
      \ are unfathomably complex and defy understanding."
    , "Before you are able to find Seth and confront him, several men and\
      \ women from the village approach you in a panic. “It’s back!” one of them\
      \ wails. You recognize him as Curtis Whateley, of the undecayed branch.\
      \ “Whatever it was that killed them Fryes, it’s back! Up and smashed the\
      \ Bishops’ home like it were made o’ paper!” Curtis and the other townsfolk\
      \ are clamoring amongst themselves, raising their voices in a panic."
    ]

introPart1 :: FlavorText
introPart1 =
  FlavorText
    Nothing
    [ "You aim to calm the townsfolk so they can explain to you what\
      \ is going on. They inform you that there was a rumbling to the north, and\
      \ when they went to investigate they found the Bishops’ farmhouse had\
      \ been torn to shreds. A trail of heavy tracks led into nearby Cold Spring\
      \ Glen. “You know what to do, right? You Arkham folk stopped that thing\
      \ last time,” one of the townsfolk says. Curtis shakes his head and bites at\
      \ his lip."
    , "“We couldn’t even see that hellish thing until the old professor sprayed\
      \ that there powder on it,” He says. “To this day, I wish I hadn’t seen it at\
      \ all…” Something must be done to stop the monster’s rampage. But, if the\
      \ documents you found are true, there may be more than one such creature\
      \ on the loose…."
    ]

introPart2 :: FlavorText
introPart2 =
  FlavorText
    Nothing
    [ "You warn the townsfolk that they are in grave danger, and urge\
      \ them to flee Dunwich while they can. Several of them immediately heed\
      \ your advice, remembering the terrible monstrosity that had previously\
      \ endangered the town. Curtis drops to his knees in despair, sweating\
      \ feverishly. “It’s that thing again, ain’t it? It’s come back fer us,” Curtis\
      \ stutters. “I hope you’ve got some of that powder the old professor had last\
      \ time. We couldn’t even see the damned thing until he sprayed it. To this\
      \ day, I wish I hadn’t seen it at all…” Something must be done to stop the\
      \ monster’s rampage. But, if the documents you found are true, there may\
      \ be more than one such creature on the loose…."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "You did all you could to stop the rampaging\
      \ monsters, but there were more of them than you realized and\
      \ you weren’t able to slay them all. Exhausted and terrified, you\
      \ retreat to Zebulon’s home and hope to survive the night."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "After slaying what seems to be the last of\
      \ the rampaging monsters you retreat to Zebulon’s home,\
      \ exhausted and rattled by your experience"
    ]
