module Arkham.Scenarios.TheMidnightMasks.Story where

import Arkham.Prelude

import Arkham.Message

data TheMidnightMasksIntroVersion = TheMidnightMasksIntroOne | TheMidnightMasksIntroTwo

introPart1 :: TheMidnightMasksIntroVersion -> Message
introPart1 version = FlavorText (Just "Part II: The MidnightMasks") body
 where
  body = case version of
    TheMidnightMasksIntroOne ->
      [ "The woman came to you in a panic, raving about monsters emerging\
        \ from the ground in a home near Rivertown. “I managed to trap them,” she\
        \ explains, “but there are others. Other pits. Other domains.” Only last week,\
        \ you would have thought she was a lunatic. Recent events, however, have\
        \ challenged your preconceptions of normality. You decide to hear her out."
      , "She introduces herself as Lita Chantler and lays out a tale that strains\
        \ the limits of your belief. “The creatures I speak of ,” she claims, “are called\
        \ ghouls—cruel beings who plague the crypts, caverns, and tunnels beneath the\
        \ city of Arkham…”"
      ]
    TheMidnightMasksIntroTwo ->
      [ "In the wake of the disaster at your home, Lita Chantler, the\
        \ red-haired woman from your parlor, lays out a tale that—even in light of\
        \ what you have just witnessed—strains the limits of your belief. “The creatures\
        \ in your home,” she claims, “are called ghouls—cruel beings who plague the\
        \ crypts, caverns, and tunnels beneath the city of Arkham…”"
      ]

introPart2 :: Message
introPart2 = FlavorText
  (Just "Part II: The MidnightMasks")
  [ "“These creatures feed on the corpses of humans, and they are served\
    \ by a dark cult within Arkham whose members have inexplicably come to\
    \ worship the ancient master of the ghouls. This cult has been killing innocent\
    \ people and feeding them to the ghouls, satiating a monstrous hunger. A dark\
    \ balance was maintained. Until now. Recently,” Lita continues, “one of their\
    \ lairs, where the corpses were stored, was destroyed. Since then, the ghouls have\
    \ been more active than usual. I have tracked their movements and tried my\
    \ best to stop them from running amok throughout the city. But I think there\
    \ is something worse going on. The cult has been planning something darker,\
    \ and more ominous, than anything I have yet observed. Indications are that\
    \ this plan shall come to fruition tonight, shortly after midnight. Beyond that, I\
    \ cannot fathom what to expect."
  , "“Many of the cultists,” Lita continues, “will seem like everyday people, despite\
    \ their foul intentions. Whenever the cult meets, its members don masks shaped\
    \ like the skulls of various animals to protect their identities from one another.\
    \ These masks are our mark. Symbols of death and decay. We must unmask the\
    \ cultists to expose and derail their plans. We have but a few hours. The more\
    \ cultists we find before midnight, the better.”"
  ]

resolution1 :: Message
resolution1 = FlavorText
  (Just "Resolution 1")
  [ "You’ve managed to obtain some useful\
    \ information about the cult and its plans. You can only hope\
    \ it’s enough."
  ]

resolution2 :: Message
resolution2 = FlavorText
  (Just "Resolution 2")
  [ "Twelve bells ring out, signaling midnight. You’re\
    \ out of time; the cult’s ritual will begin shortly. You’ve managed\
    \ to obtain some useful information about the cult and its plans.\
    \ You can only hope it’s enough."
  ]
