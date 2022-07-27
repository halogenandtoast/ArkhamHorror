module Arkham.Scenarios.TheUntamedWilds.Story where

import Arkham.Prelude

import Arkham.Message

intro :: Message
intro = FlavorText
  (Just "Scenario I: The Untamed Wilds")
  [ "Sunday, June 28th, 1925"
  , "It’s been a hell of a day. We’ve just set up camp along the northern border\
    \ of the rainforest where Alejandro believes the Eztli once dwelled. The\
    \ jungle here is dense and untamed. The sky hides above the tree cover, but I\
    \ can tell from the crisp breeze that storm clouds are brewing overhead."
  , "The dangers of our expedition have been explained to us many times.\
    \ We are hundreds of miles away from any sign of civilization. Nobody in\
    \ modern times has yet to pierce the heart of these wilds. Until now, nobody\
    \ has had reason to try. After all, it wouldn’t make sense to find Aztec ruins\
    \ in this region of Mexico. And yet, here we are: looking for ruins that may\
    \ not exist in a place where, by all accounts, they should not."
  , "Already one of our surveyors has fallen ill, and another has been bitten by\
    \ a viper. I’m starting to think the rainforest doesn’t want us here. If this luck\
    \ persists, ours will be the shortest expedition in Miskatonic University’s\
    \ history. Tonight, we rest. Tomorrow, we venture into uncharted wilds."
  ]
