module Arkham.Scenarios.TheMidwinterGala.FlavorText where

import Arkham.Prelude

import Arkham.Text

intro1 :: FlavorText
intro1 =
  FlavorText
    (Just "The Midwinter Gala")
    [ BasicEntry
        "A light snow falls as you step out of your taxi in front of the cracked and weathered facade of the old Kingsport manor. Several other guests—some of the finest movers and shakers from Arkham to New York—flitter past you to present their invitations to the doorman. As you follow them in, you catch a scent of cedar and sweet florals, with an undercurrent of something rank."
    , BasicEntry
        "\"The Lantern Club is thrilled to have you,\" a grinning, sharp-eyed attendant says as they hand you the same bone-white mask worn throughout the soirée. Feeling out of place, you don your mask and follow an elderly flapper into a drawing room with a crackling fire. The room would be warm and inviting if not for an uncanny strangeness in the dreamlike paintings and ghastly sculptures. Looking at them makes your head swim."
    , BasicEntry
        "\"Please, please, if you’ll indulge me.\" A stentorian voice bellows from an adjoining room. You follow the voice back to the lavish foyer. At the top of the stairway, a stately gentleman with a finely trimmed beard and alabaster mask raises his champagne glass."
    , BasicEntry
        "\"I am Declan Pearce, nomarch of the Lantern Club. Follow me please.\" The guests follow him through a wide set of double doors into the ballroom, in the center of which a gleaming gem is displayed behind a thick pane of glass. The socialites gather around, marveling at the swirling, iridescent colors that move like clouds within the hard cut angles of the Jewel."
    , BasicEntry
        "\"My honored guests. This is the Jewel of Sarnath,\" Declan Pearce says triumphantly. \"A stone unlike any other, rumored to have been cut in an ancient city on distant shores. Legends claim it can grant its bearer’s wishes.\" Scattered laughter follows Pearce’s remark, and the chairman himself suppresses a chuckle. \"Yes, yes, I know. Of course, the real purpose of this evening is you. We invited only the best, the most supreme of New England’s social strata, to appeal to your sense of charity. The most distinguished—or generous—among you may be permitted to hold the Jewel by the end of the evening. And who knows? Perhaps dreams are real, and the Jewel of Sarnath can grant wishes. But until then, enjoy our little party.\""
    , BasicEntry
        "After another wave of friendly laughter, Pearce claps his hands and a dozen attendants bearing mouth-watering appetizers descend upon the room. Your eyes scan the ballroom, hoping for some sign of the contact who led you here. A chill runs down your spine as you lock eyes with a tall, masked figure standing at the back of the crowd. He slowly cocks his head just a little too far to the right. You wonder if he is smiling behind his bone-white mask."
    , BasicEntry
        "After what feels like ages, the man breaks eye contact with you to continue scanning the room. Suppressing a shiver, you search the gaggle of guests. Your gala invitation arrived in an unmarked parcel, along with a note promising generous compensation for your assistance in procuring the Jewel of Sarnath. At long last, you find your mysterious contact."
    ]

guestChoice :: FlavorText
guestChoice =
  FlavorText
    Nothing
    [ BasicEntry
        "Who is your mysterious contact? The investigators must choose which faction requested their assistance. (Hint: The faction you choose will determine your objectives in this scenario.)"
    ]

guestTheFoundation :: FlavorText
guestTheFoundation = FlavorText Nothing [BasicEntry "A severe woman in a well-tailored suit leans against the wall with her arms crossed. She looks like she’d rather be in a shootout than at this elaborate party."]

guestMiskatonicUniversity :: FlavorText
guestMiskatonicUniversity = FlavorText Nothing [BasicEntry "An astute professorial type strokes his goatee as he studies  a painting. You recognize him as Caldwell Phillips, the dean of Miskatonic University."]

guestTheSyndicate :: FlavorText
guestTheSyndicate = FlavorText Nothing [BasicEntry "A slick-looking man with dark hair taps ash from his cigar into a hapless guest’s champagne flute. His darting eyes suggest he’s casing the joint."]

guestTheSilverTwilightLodge :: FlavorText
guestTheSilverTwilightLodge = FlavorText Nothing [BasicEntry "You catch the gleam of a signet ring in the corner of your eye. Turning, you see the prestigious leader of the Silver Twilight Lodge, Carl Sanford, conversing with a pair of masked guests."]

guestLocalsOfKingsport :: FlavorText
guestLocalsOfKingsport = FlavorText Nothing [BasicEntry "A tall, stout man with a long black beard and a weatherbeaten coat looks clearly out of place at this extravagant gala. You recognize him from The Rope and Anchor tavern in Kingsport."]

introTheFoundation :: FlavorText
introTheFoundation =
  FlavorText
    (Just "The Foundation")
    [ BasicEntry
        "\"Ah, you came,\" the woman says in a light Russian accent. \"I am agent Valeriya Antonova. I am here on behalf of the authorities.\" When you ask which authorities, she simply nods. \"Yes. The authorities.\" She explains that she and her \"authorities\" are investigating strange disappearances in the area and abroad, and they suspect the Lantern Club and Declan Pearce are to be blamed. The agent seems to loosen up as she knocks back a flute of champagne. \"We also believe this Jewel may be the source of some highly unusual paracausal phenomena. Be prepared for anything.\""
    ]

introMiksatonicUniversity :: FlavorText
introMiksatonicUniversity =
  FlavorText
    (Just "Miskatonic University")
    [ BasicEntry
        "You approach Dean Phillips and introduce yourself. The painting he was examining is eerily lifelike, depicting a humanoid creature covered in fur, with multiple arms, each with a clawed talon. \"Quite imaginative, these Lantern Club folks,\" he laughs. \"I see you received my missive. Thank you for coming. I must admit, the university is more than a little curious to study this ancient gem. The Jewel was rumored to have been housed in a Tibetan monastery until it vanished in the twelfth century. I simply couldn’t pass up the opportunity!\" he exclaims. \"Of course, if there were some way to examine the Jewel more thoroughly…\""
    ]

introTheSyndicante :: FlavorText
introTheSyndicante =
  FlavorText
    (Just "The Syndicate")
    [ BasicEntry
        "The man raises his glass in a mock toast as you approach. \"Johnny Valone. Glad you could make it to this mutually beneficial business arrangement.\" The man finishes his drink and casually hands the empty glass to a nearby socialite, who looks equal parts confused and offended. Valone is seemingly deaf to their protest as he leads you to a deserted study. \"Your reputation precedes you. Or rather, it doesn’t precede you. Nobody’ll suspect your involvement, which is why I’ve asked for your help in procuring this Jewel of Something-Or-Another. It’s a big score. I can promise you we’ll make it worth your while.\""
    ]

introTheSilverTwilightLodge :: FlavorText
introTheSilverTwilightLodge =
  FlavorText
    (Just "The Silver Twilight Lodge")
    [ BasicEntry
        "Carl Sanford makes no introduction as you approach. \"Very good,\" he says matter-of-factly, then waves his masked guests away. \"As I am sure you are aware, rumors of the Jewel of Sarnath are quite prevalent amongst the occult circles of Arkham. Given the Jewel’s…colorful history, it would make quite the addition to the Lodge’s collection.\" The patriarch stares down his nose at you. \"Although I suspect that there is far more at play here than meets the eye. That is why I have asked you here. Acquire the Jewel for the Lodge, and you shall be rewarded with knowledge and riches beyond your wildest dreams.\" You ask him why the Lodge deserves the Jewel more than any other, and he smirks. \"We will keep it safe. Far safer than it would be in the hands of others. You cannot even begin to comprehend its power.\""
    ]

introLocalsOfKingsport :: FlavorText
introLocalsOfKingsport =
  FlavorText
    (Just "Locals of Kingsport")
    [ BasicEntry
        "\"The name’s Bain. William Bain,\" the stalwart man shakes your hand with a firm grip. \"I’m glad you came to this fancy little party. Truth be told, I’m not really a local; I just happen to have a soft spot for this old port.\" The man deftly avoids your questions about his origins before pointing at the silent masked figure in the corner of the room. \"He is the reason I invited you here. They call him ‘The Bloodless Man.’ Whatever the Lantern Club has planned for this evening is certainly his design.\" The captain sighs. \"No good can come of that strange Jewel. Only ruin. At least, so long as the Lantern Club has it.\" After further discussion, you agree to help Bain recover the Jewel."
    ]

noResolution :: FlavorText
noResolution =
  FlavorText
    Nothing
    [ BasicEntry
        "You wake up, freezing cold, facedown in the snow outside the Lantern Club’s stately manor."
    , BasicEntry
        "The house is deserted, the cheery windows dark and foreboding, like the empty eye sockets of a grinning skull."
    , BasicEntry
        "You can’t remember how you escaped the mayhem, but you will never forget what happened at Kingsport’s first—and hopefully last—Midwinter Gala."
    ]

noResolutionResigned :: FlavorText
noResolutionResigned =
  FlavorText
    Nothing
    [ BasicEntry
        "You wake up, freezing cold, facedown in the snow outside the Lantern Club’s stately manor."
    , BasicEntry
        "The house is deserted, the cheery windows dark and foreboding, like the empty eye sockets of a grinning skull."
    , BasicEntry
        "You can’t remember how you escaped the mayhem, but you will never forget what happened at Kingsport’s first—and hopefully last—Midwinter Gala."
    ]

resolution1 :: FlavorText
resolution1 =
  FlavorText
    (Just "Resolution 1")
    [ BasicEntry
        "You look up from the Jewel to see your contact standing in front of the manor, silhouetted in the moonlight."
    ]

resolution2 :: FlavorText
resolution2 =
  FlavorText
    (Just "Resolution 2")
    [ BasicEntry
        "Agent Antonova turns and strides toward you, her left cheek spattered with viscous ichor."
    , BasicEntry
        "\"I did not like this party at first, but it became much more interesting once they locked the doors,\" she grins, then lays a hand on your shoulder."
    , BasicEntry
        "\"I have reported the strange effects of this ‘Jewel’ to my superiors. They want it brought to our offices for safekeeping, but I have several other obligations to attend to before I can deliver it.\""
    , BasicEntry
        "\"I suppose there is little harm in letting you hold onto it for now. Know that I will be back later to collect it.\""
    , BasicEntry
        "After seeing the chaos the Jewel brought to the Gala, you wonder if you are being used more as a ‘case study’ than a recipient of its power."
    ]

resolution3 :: FlavorText
resolution3 =
  FlavorText
    (Just "Resolution 3")
    [ BasicEntry
        "After calming down a very flustered Dean Caldwell, the academic grabs the Jewel from your hand and holds it up to the light."
    , BasicEntry
        "\"Truly remarkable. The legends hardly do it justice. Miraculous! Breathtaking!\""
    , BasicEntry
        "The astute gentleman eventually runs out of descriptors, then thanks you for your assistance."
    , BasicEntry
        "\"Given what happened here tonight, I would prefer we not house the Jewel on campus grounds. We only just rebuilt the student dormitories. But if you could bring it by my office next week, I would relish the chance to study it further.\""
    ]

resolution4 :: FlavorText
resolution4 =
  FlavorText
    (Just "Resolution 4")
    [ BasicEntry
        "\"Not too shabby,\" says Johnny Valone, lit cigar in his mouth. \"You have light hands. Could always use another pair for a job or two.\""
    , BasicEntry
        "He winks as he blows smoke out the side of his mouth. \"I figured we could pawn the thing on the black market for a fair price, but after seeing what happened in there–\" his tough exterior softens. \"I wouldn’t wish that on anyone. What were those things anyway? Elephants? Sick gorillas?\""
    , BasicEntry
        "He takes one last puff of his cigar. \"I guess I’ll never know. All I knows is, there’s no way I’m hauling that rock back to Arkham. Consider the Jewel a party favor. And this is for savin’ my ass.\" He hands out a wad of bills to you and melts into the shadows with a Cheshire grin."
    ]

resolution5 :: FlavorText
resolution5 =
  FlavorText
    (Just "Resolution 5")
    [ BasicEntry
        "\"I don’t care what it takes: I want that Jewel and I want it now!\" Carl Sanford seethes at his lackeys."
    , BasicEntry
        "As you listen to the patriarch excoriate his associates, you think back to the events of the evening. Although you had resolved to help the Lodge, is the world truly safer with the Jewel in their possession?"
    , BasicEntry
        "Rather than return it to Sanford, you pocket the Jewel and slip away into the evening. You are the only one who can temper its power. What you are doing truly is for the greater good. Hopefully Sanford will never find out."
    ]

resolution6 :: FlavorText
resolution6 =
  FlavorText
    (Just "Resolution 6")
    [ BasicEntry
        "\"You’ve done a great thing this evening,\" Captain Bain claps you on the back. \"Given tales of the Jewel, this evening could have gone far worse. It is said that the Jewel brings the greatest dreams and worst nightmares of its bearer to life. Perhaps it is fitting, what happened to the Lantern Club…\""
    , BasicEntry
        "Bain looks up at the darkened windows and snow stained with blood, then presses the Jewel back in your hand. \"Something tells me, however, that with you as its bearer, the Jewel may do some good.\""
    , BasicEntry
        "You lose yourself looking into the cloudy, smoky heart of the gem. When you look up, the Captain has vanished."
    ]

resolution7 :: FlavorText
resolution7 =
  FlavorText
    (Just "Resolution 7")
    [ BasicEntry
        "Your skin prickles as you sit in the back seat of your cab, and you have the feeling of being watched."
    , BasicEntry
        "You turn to look back at the manor’s facade to see the nightmarish face of the Bloodless Man staring down at you from the window."
    , BasicEntry
        "As the manor disappears from view, you think back to Kingsport, a city shrouded in mystery and folklore. The city beckons with its name alone, conjuring up images of misty sea cliffs, shadowy coves, and ancient, brooding mansions."
    , BasicEntry
        "For generations, stories of inexplicable phenomena, strange dreams, and haunting legends have plagued Kingsport. After tonight’s events, you finally understand why."
    ]

