module Arkham.Scenarios.TheDoomOfEztli.Story where

import Arkham.Prelude

import Arkham.Message

intro1 :: FlavorText
intro1 =
  FlavorText
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

intro2 :: FlavorText
intro2 =
  FlavorText
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

defeat :: FlavorText
defeat =
  FlavorText
    (Just "Investigator Defeat")
    [ "The temple shakes with the ferocity of an earthquake, causing you to collapse\
      \ to the ground. Serpent creatures emerge from the shadows around you,\
      \ surrounding you completely. The thought, “This is how it ends,” flashes through\
      \ your mind. For a moment, you contemplate the absurdity of your death. You can\
      \ only hope that nobody else will be foolish enough to try to explore these\
      \ forsaken ruins."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ "(The following is scrawled below the previous journal entry.)"
    , "The outside of the temple ruins appeared Aztec in origin, but once we breached\
      \ the interior, it became clear that these buildings had a very different and\
      \ inhuman origin. The ruins were alive. The grooves in the walls were illuminated\
      \ as though powered by electricity, and a faint hum of energy reverberated\
      \ throughout the halls. In spite of the serpent creatures, we were able to reach\
      \ a hidden passage that led deeper underground."
    , "In the central chamber of these underground halls, we found our prize: a steel\
      \ orb of unknown origin, gently humming and glowing with a dim blue light. It was\
      \ attached delicately to a bronze frame, appearing almost religious in its\
      \ significance, and we knew it could not have been made by the Aztecs."
    , "With the artifact in tow, we fled the ruins. The building had started to\
      \ crumble around us, as though the orb was the only thing keeping it standing.\
      \ Had it been powering the ruins, like a battery? What exactly was this device\
      \ that the serpents guarded so fervently?"
    , "Maria was waiting for us with the trucks when we finally reached the northern\
      \ edge of the rainforest. We’re getting the hell out of this serpent-infested\
      \ jungle while we still can."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ "(The following is scrawled below the previous journal entry.)"
    , "It is not in my nature to leave a thing unfinished. For the sake of my fallen\
      \ comrades, I have taken it upon myself to record the remainder of our expedition\
      \ in this journal."
    , "The others never made it out of the temple. I convinced two of the other\
      \ members of the expedition, José and Andrea, to venture in after the\
      \ others...But now I fear a terrible fate has befallen them, as well."
    , "Thankfully, my gambit was worthwhile. While José and Andrea searched for the\
      \ others, I snuck into the temple and took a different route. Having seen what we\
      \ have seen, knowing what we know now, I couldn’t have returned to the university\
      \ without evidence of our discovery. While the serpent creatures were distracted,\
      \ I was able to reach a hidden passageway that led deeper underground."
    , "In the central chamber of these underground halls, I found the device. The orb\
      \ was made of an unnatural steel, attached delicately to its bronze frame. It\
      \ pulsed with power in my hands and glowed with a dim blue light. Any fool could\
      \ tell that it was not made by the Aztecs, but the other members of the\
      \ expedition could not even begin to understand its true significance. The ruins\
      \ started to crumble around me as I left, but it mattered not. The relic was all\
      \ that mattered now."
    , "With the artifact in tow, I returned to camp and told the others it was time to\
      \ depart. Maria was waiting for us with the trucks when we finally reached the\
      \ northern edge of the jungle. Now we are headed to Texas, where we will cross\
      \ the border into the United States."
    , "It is a shame what happened to the others. Their sacrifice will not be in vain.\
      \ Now, my work can continue."
    , "– Alejandro Vela"
    ]

resolution3 :: FlavorText
resolution3 =
  FlavorText
    (Just "Resolution 3")
    [ "(The following is scrawled below the previous journal entry.)"
    , "This was a mistake. This was all a terrible mistake. We escaped the temple with\
      \ our lives, but not a moment too soon. The ruins were alive. It was as though\
      \ the walls knew of our intrusion, hated us for our presence. Wanted us out.\
      \ Wanted us dead."
    , "We have gathered outside the temple where José is waiting for us. Alejandro,\
      \ unwilling to return home empty-handed, wants us to go back inside. We must\
      \ decide what to do next."
    ]

resolution4 :: FlavorText
resolution4 =
  FlavorText
    (Just "Resolution 4")
    [ "(The following is scrawled below the previous journal entry.)"
    , "We’ve made our decision. Alejandro is right: we’ve come too far to back down\
      \ now. Our plan is to regroup and head back into the temple once more. Even from\
      \ here, we can tell that the layout of the temple has warped and changed. The\
      \ ruins seem to have reset to their dormant state. With any luck, we’ll make it\
      \ out this time..."
    ]

resolution5 :: FlavorText
resolution5 =
  FlavorText
    (Just "Resolution 5")
    [ "(The following is scrawled below the previous journal entry.)"
    , "We made our decision. There was no way we were going back into that deathtrap.\
      \ Thankfully, there was another way. José had brought some TNT from the trucks,\
      \ and there was more than enough to level the damned place."
    , "That night, we put on a hell of a fireworks show. The jungle protested. Birds\
      \ scattered into the sky, the ground rumbled, and hateful snarls surrounded our\
      \ camp. But the dynamite did its job. Once the dust settled, we began sifting\
      \ through the debris in search of anything with historical or cultural\
      \ significance we could bring back to Arkham."
    , "José was the first to notice the blue light peeking out from underneath the\
      \ remains of the temple. The rays grew in intensity with each passing moment. As\
      \ we started clearing the rubble in order to discover the source of the light,\
      \ there was a sudden flash, and the debris at the center of the ruins scattered.\
      \ A steel orb of unknown origin lay on top of the rubble, gently humming. It\
      \ was attached delicately to a bronze frame, appearing almost religious in its\
      \ significance. The blue glow emanating from the device dimmed as I touched it.\
      \ There was no way a device like this was built by the Aztecs."
    , "With the artifact in tow, we left the smoldering temple behind us. We had\
      \ outstayed our welcome in this rainforest, and we knew it. Around us, the jungle\
      \ hissed. The ground slithered and crawled with snakes. In the darkness, we heard\
      \ shouts in a language we could not understand. There was no time to pack up\
      \ camp; we ran as soon as we had the chance. Andrea was struck in her throat by a\
      \ black-feathered arrow, her startled scream captured in grim perpetuity. José\
      \ was grabbed by a giant boa as we fled north into the river canyon. His neck\
      \ snapped before we could reach him."
    , "Maria was waiting for us with the trucks when we finally reached the northern\
      \ edge of the jungle. We didn’t stop until we crossed into Mexico City. Even now\
      \ I do not feel safe: who could, after seeing what we have seen?"
    , "To hell with the Eztli and to hell with this forsaken place."
    ]
