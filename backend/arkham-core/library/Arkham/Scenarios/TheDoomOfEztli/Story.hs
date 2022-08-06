module Arkham.Scenarios.TheDoomOfEztli.Story where

import Arkham.Prelude

import Arkham.Message

intro1 :: Message
intro1 = FlavorText
  (Just "Intro 1")
  [ "Wednesday, July 8th, 1925"
  , "Our runners, José and Maria, returned several days later. They were fully\
    \ stocked with food, water, aspirin, and weapons. I am curious how deep\
    \ Alejandro’s connections are in Mexico. For an outcast historian, he was able to\
    \ secure guns and ammunition faster than I had anticipated. But perhaps it is\
    \ better if he keeps his sources undisclosed."
  , "We ventured once more into the jungle. This time, we were prepared.  We had\
    \ learned from our previous excursion, and we made our way across the river\
    \ canyon, venturing deeper south than we had dared before. Eventually we reached\
    \ the ruins Alejandro sought, and soon we will enter what Alejandro believes is\
    \ the main temple. Strangely, the serpent creatures we encountered earlier didn’t\
    \ attack in force as we had feared. Could they have fled? Or are they simply\
    \ lying in wait, preparing to ambush while we sleep at night?"
  ]

intro2 :: Message
intro2 = FlavorText
  (Just "Intro 2")
  [ "Friday, July 3rd, 1925"
  , "We spent most of yesterday moving our camp to the edge of the ruins. With the\
    \ serpentine creatures still guarding the region, we have decided that a small\
    \ party will have the best chance of slipping into the main temple unnoticed. I\
    \ told Maria, the expedition’s naturalist, to take one of the trucks and wait\
    \ beyond the northern edge of the rainforest. Our cartographer, José, is guarding\
    \ the camp until we return. We’re on our own, but at least we’ve cleared an\
    \ escape route in the event things go sour..."
  ]
