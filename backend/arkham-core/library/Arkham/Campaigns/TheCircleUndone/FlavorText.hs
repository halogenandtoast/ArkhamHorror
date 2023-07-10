module Arkham.Campaigns.TheCircleUndone.FlavorText where

import Arkham.Prelude

import Arkham.Message

prologue :: FlavorText
prologue =
  FlavorText
    (Just "Prologue")
    [ "Sunday, November 22nd, 1925. Arkham, Massachusetts."
    , "Though All Hallows’ Eve is nearly a month past, a grim melancholy lingers\
      \ throughout the town. Each morning, a thick fog permeates the streets. Nights\
      \ are beginning to grow longer, and if you ask around town, you’ll hear people\
      \ swear that it’s getting darker, too. But despite the gloomy mood, progress\
      \ continues in the sleepy town of Arkham. The election of Nathaniel Rhodes to the\
      \ United States Senate has upstanding members of the community feeling optimistic\
      \ about the town’s future. And tonight, at his well-appointed estate in French\
      \ Hill, a man named Josef Meiger hosts the Silver Twilight Lodge’s charity gala,\
      \ an annual members-only event that will turn deadly for several attendees..."
    ]

intro :: FlavorText
intro =
  FlavorText
    (Just "Intro")
    [ "“Ladies and gentlemen,” Josef Meiger announces, raising a glass of champagne in\
      \ a toast. A hush descends on the room, until only the crackle of the fireplace\
      \ and whispers of gossip can be heard. “Allow me to welcome you all into my home\
      \ for this year’s charity gala. We have some very upstanding citizens here\
      \ tonight, and I thank all of you for your hard work and generosity.” Cheers and\
      \ murmurs of agreement fill the room. Many of the guests raise their glasses to\
      \ Valentino, one of the most esteemed members of the Lodge this year, who sits at\
      \ the Guest of Honor table, which is nearest to the fireplace."
    , "Josef ’s assistant, Jerome, blends into the wall behind Josef, discreetly\
      \ checking his pocket watch. In another corner of the room, the head housekeeper,\
      \ Penny, walks from table to table, filling empty glasses and collecting dirty\
      \ salad plates. “Each of you has done great deeds in the name of the Silver\
      \ Twilight Lodge and our historic city,” Josef continues. “Next year, we will\
      \ continue to shoulder this burden and do what must be done for the sake of\
      \ progress.” Jerome steps forward quietly, interrupting Josef ’s speech with the\
      \ unassuming confidence that comes from years of trusted service. He taps Josef\
      \ lightly on his shoulder and shows him the time. “I’m afraid I am already out of\
      \ time. Thank you all very much for attending,” Josef concludes, bowing. Polite\
      \ applause rises from the audience, and Josef walks briskly toward the parlor,\
      \ followed closely by his assistant. Two servants collect coats as latecomers\
      \ trickle into the manor, and Gavriella—Josef ’s head of security—watches over\
      \ the entrance with hard eyes and a clenched jaw. “Has Mr. Sanford arrived?”\
      \ Josef asks curtly, tapping his polished leather shoe on the floor."
    , "“I’m afraid not,” Jerome replies, flipping through the last pages of the\
      \ estate’s guest book. “But he should be here any minute, Mr. Meiger.”"
    , "“Good. I want there to be no problems whatsoever when he arrives; am I\
      \ understood?” Josef calls out to Gavriella: “Make sure he is well protected.”\
      \ Gavriella nods, patting the handle of her .45 in her shoulder holster. Josef\
      \ turns his attention back to his assistant. “And have Penny make sure the main\
      \ course is kept good and hot while we wait for Mr. Sanford’s arrival. Not a\
      \ single slice is to be served without his presence.”"
    , "“Not even for Mr. Rivas, sir?” Jerome asks, glancing at Josef over the rim of\
      \ his thick glasses. Josef pauses for a moment, considering."
    , "“Pour Mr. Rivas another glass of champagne, and I am sure he will not complain.\
      \ Also, I’m still waiting on those accounts I asked you about earlier today.\
      \ Don’t forget,” Josef says, clapping his assistant on the shoulder before\
      \ walking back into the banquet hall. Jerome nods obediently and heads upstairs."
    , "Soon after, the dark mist would appear, and nothing would be the same."
    ]

gavriellaIntro :: FlavorText
gavriellaIntro =
  FlavorText
    (Just "Gavriella Intro")
    [ "You stand guard as instructed, waiting for Mr. Sanford to appear. By now, the\
      \ remaining guests have filed into the banquet hall, and you can hear the sounds\
      \ of merriment and drinking coming from beyond the wooden door behind you. You\
      \ pay them no mind, remaining vigilant. Years of fighting and discipline have\
      \ taught you to be ready for anything, even at a harmless banquet like this. Just\
      \ as you begin to ponder whether your talents are being wasted under Mr. Meiger’s\
      \ employ, a dark mist invades the parlor through the front door and the window\
      \ frames, flooding the room. At first, you believe it to be only the evening fog\
      \ seeping through the manor’s entryway, until you begin to notice that everything\
      \ the mist touches seems to have decayed as though aged hundreds of years."
    , "You step back cautiously, keeping a hand on the grip of your weapon just in\
      \ case. Never in all your years have you seen something like this. An unnatural\
      \ chill spreads throughout the room, and shivers run up your spine. As the ashen\
      \ mist finishes pouring in, it coalesces into a singular form: a humanoid figure\
      \ wrapped in shadows. It raises its hand and points at you with a charred,\
      \ blackened finger."
    , "You unholster your firearm and point it at the creature, allowing your training\
      \ to take over. “Don’t come any closer!” you shout. The thing watching you from\
      \ the entrance is unfazed. Its ethereal form begins to glide toward you, dark\
      \ mist crawling over the carpet in its wake.  “I warned you,” you growl, and a\
      \ thunderous shot echoes through the parlor as you squeeze the trigger. The\
      \ bullet rips a hole in the figure’s head like a rush of air billowing through a\
      \ column of smoke. The mist stitches itself together, and the thing continues to\
      \ drift your way, reaching out menacingly."
    , "Nothing could have prepared you for combat with such an unnatural enemy. Faced\
      \ with no other option, you turn and flee up the staircase nearby, pausing to\
      \ squeeze off several more shots at the top. The bullets that make their target\
      \ simply pass harmlessly through the ghostly figure, striking the door behind it.\
      \ A few stray shots shatter a column of the staircase’s wooden balustrade."
    ]

jeromeIntro :: FlavorText
jeromeIntro =
  FlavorText
    (Just "Jerome Intro")
    [ "You carefully flip through the pages of Mr. Meiger’s ledger, looking for the\
      \ accounts he inquired about. You have served Mr. Meiger faithfully for almost a\
      \ decade, and he trusts you with sensitive information like this—a point of pride\
      \ for you. While you are often curious about your employer’s business, you have\
      \ never pried into his personal matters. Not until tonight, anyway."
    , "You adjust your glasses and lean forward as you turn to the page regarding Mr.\
      \ Meiger’s request. Some of the names on the list you recognize: Rivas, Gensler,\
      \ Fairmont, Rhodes, Wick. But many are names you have never heard of before, let\
      \ alone seen affiliated with Mr. Meiger’s work: Lindquist, Konstantinov, Magro,\
      \ Atkinson, Lamar...Just how deep do Mr. Meiger’s connections go?"
    , "Strange as that may seem, it is the list of names on the page afterward that\
      \ raises your hackles. While it was clear that the names on the previous page are\
      \ associates of Mr. Meiger’s, or at least prominent members of the Lodge, you can\
      \ only assume that this next series of names is of people your employer\
      \ is...targeting. For what, you cannot say."
    , "You stand next to Josef ’s desk and record the list in your pocket journal\
      \ carefully, making sure to keep the names in the exact order they appear in Mr.\
      \ Meiger’s ledger. You hope that your suspicion is nothing more than the absurd\
      \ imagination of an overworked secretary. Still, something about all of this has\
      \ you concerned. That, and the sudden draft of frigid air that has somehow wafted\
      \ into the room. Your gaze naturally drifts to the window, at which point you\
      \ scream out in shock and lose your balance, stumbling backward into Mr. Meiger’s\
      \ desk."
    , "Pressed up against the office window is a host of screaming faces emerging from\
      \ the mist, or perhaps composed of it. Their ghostly hands press against the\
      \ glass, their eyes hollow and empty. Your reading glasses clatter to the ground\
      \ and shatter under your heel as you scramble to the other side of the office.\
      \ You don’t realize that you dropped your pocket journal in the chaos until it is\
      \ too late."
    ]

pennyIntro :: FlavorText
pennyIntro =
  FlavorText
    (Just "Penny Intro")
    [ "You sigh as the cool New England air embraces you. It has been a very stressful\
      \ night: stressful enough that you’ve decided to sneak away from the clamor to\
      \ grab a quick smoke before heading back to work. You rest your arms along the\
      \ metal railing of the balcony, your fingers trembling as they hold your\
      \ cigarette. The dreary gambrel rooftops and Victorian manors of French Hill span\
      \ the view below you, the unlit windows hiding countless secrets."
    , "You can’t remember a night in which your employer was as concerned with every\
      \ little detail as he is tonight. He is normally so calm, so collected. There’s\
      \ something special about tonight that has him on edge. Is it Mr. Sanford? A\
      \ shudder crawls through you. For some reason, something about that man gives you\
      \ the creeps. But Josef has interacted with Mr. Sanford many times before, so\
      \ that cannot be the reason. What could it be that has Mr. Meiger so worried?"
    , "You are torn from your thoughts by the gathering of grey clouds overhead,\
      \ swiftly blotting out the night sky. As the clouds grow ever closer, you begin\
      \ to see shapes emerging from the vapor. You squint and lean over the railing for\
      \ a better view. That is when you notice the true forms of the shapes in the\
      \ mist: their screaming faces, their clawing hands, all writhing in torment and\
      \ coming straight for you. You cry out in terror and back up against the wall as\
      \ the mist envelops the building. A spectral shape emerges from the haze beyond\
      \ the railing, dressed in bloodstained rags. It begs for rest in a croaking,\
      \ gasping voice. Your only hope for escape is to retreat back into the manor and\
      \ call for help..."
    ]

valentinoIntro :: FlavorText
valentinoIntro =
  FlavorText
    (Just "Valentino Intro")
    [ "“Are you going to break, or are you going to sit there admiring the cue ball\
      \ all night?” you ask with a mocking smile. Your opponent sets the ivory ball\
      \ back on the table with a sigh."
    , "“I can’t help myself around ivory. You know that, Tino,” Adam replies. You\
      \ rarely see Adam Gensler except during Lodge functions, but you prefer his\
      \ company to that of the stuffy business types who make up most of the Lodge\
      \ these days. He makes his shot, the sudden clattering interrupting the din of\
      \ conversation. “Your shot, Tino. And do try to avoid hitting the nine ball\
      \ first. You’re always so overeager.” Adam chuckles as he moves to the other side\
      \ of the table, making way for you."
    , "“That was one time, my friend, one time.” You roll your eyes. Truth be told,\
      \ you’ve enjoyed coming to these events for the opportunity to give back to the\
      \ community, not to hobnob with Arkham’s gentry. But hobnobbing has had its\
      \ perks."
    , "You lean over the table and clear your mind as you line up your shot. The room\
      \ falls silent as you block out the clamor and the music, focusing all of your\
      \ attention on the one ball and the corner pocket. You hold your breath and\
      \ strike the cue ball. It clacks as it hits its target, and the one ball drops\
      \ effortlessly into the corner pocket. “There, you see?”"
    , "It is only then that you realize the silence around you is not just in your\
      \ mind. All of the light and warmth has been sucked out of the room. Adam is\
      \ gone, along with everyone else who was present just moments before you took\
      \ your shot. “Hello?” you call out. A dark mist coils about your ankles as you\
      \ walk around the billiards table. “If this is a practical joke of some kind,\
      \ it’s in awfully poor taste,” you remark. The only response is the deep growl\
      \ that emerges from beneath the table. Morbid curiosity compels you to look\
      \ underneath it, at which point a dark hound pounces onto your chest, savagely\
      \ clawing at your torso. You react instinctively, pushing it off of your body as\
      \ hard as you can. It lands on the billiards table, causing it to collapse under\
      \ the creature’s weight. You slowly back up as the creature rises to its feet\
      \ once again and jumps off the broken table, its hollow eyes gazing into your\
      \ soul."
    ]

thePriceOfProgress1 :: FlavorText
thePriceOfProgress1 =
  FlavorText
    (Just "The Price of Progress 1")
    [ "“Ah, it’s you. I did not expect you to be here, though perhaps I should have,”\
      \ Mr. Sanford says with a hint of surprise in his voice. “I regret that you had\
      \ to be involved in this unfortunate affair. I did not plan for you to be a part\
      \ of any of this.” You narrow your eyes. You suspect fortune had nothing to do\
      \ with it. You ask him what will happen to those still trapped inside. “Human\
      \ progress requires sacrifice,” he recites stoically. “It is lamentable when that\
      \ sacrifice is in blood, but the price in lamentation does not outweigh the yield\
      \ of our labor.”"
    ]

thePriceOfProgress2 :: FlavorText
thePriceOfProgress2 =
  FlavorText
    (Just "The Price of Progress 2")
    [ "“I regret that we must meet under such... unfortunate circumstances,” Mr.\
      \ Sanford says offhandedly. You narrow your eyes. You suspect fortune had nothing\
      \ to do with it. You ask him what will happen to those still trapped inside.\
      \ “Human progress requires sacrifice,” he recites stoically. “It is lamentable\
      \ when that sacrifice is in blood, but the price in lamentation does not outweigh\
      \ the yield of our labor.”"
    ]

thePriceOfProgress3 :: FlavorText
thePriceOfProgress3 =
  FlavorText
    (Just "The Price of Progress 3")
    [ "You didn’t come here to listen to Mr. Sanford proselytize about sacrifice. You\
      \ came here for answers. You demand an explanation, but the elderly man cuts you\
      \ off with a scowl. “Yes, yes. You speak of the incident last week. That was the\
      \ creature’s first manifestation, and the arrival of the dark mist, which you\
      \ encountered inside.” He glances at the front door to the manor and clears his\
      \ throat. “The Lodge was not involved in the disappearances that occurred that\
      \ night,” he explains after a short pause. “But we couldn’t exactly go to the\
      \ police and tell them a creature made of mist kidnapped four people in Mr.\
      \ Meiger’s household, could we? So, we had to take matters into our own hands. We\
      \ suspected that creature was drawn to the crowd, so we recreated the incident in\
      \ order to understand what happened—to discern its motives.” The decision to host\
      \ another event at Mr. Meiger’s estate so soon after the disappearances makes\
      \ sense to you now. As much as you hate to admit it, there is some logic to the\
      \ plan the Lodge put into action, although you don’t particularly like being used\
      \ as bait. Who knows how many innocent bystanders were put in mortal danger? Even\
      \ if the goal was noble, was it worth the sacrifice?"
    , "“I hope you understand that our organization only seeks the betterment of\
      \ humankind. There are harsh truths that lie beyond our five senses,” Mr. Sanford\
      \ says, his expression grim. “If we are to survive and prosper, we must adapt. We\
      \ must learn. We must understand. That is what our order seeks to achieve—a\
      \ greater understanding of the world around us. A worthy pursuit, don’t you\
      \ agree?”"
    ]

thePriceOfProgress4 :: FlavorText
thePriceOfProgress4 =
  FlavorText
    (Just "The Price of Progress 4")
    [ "Before you can reply, one of Sanford’s men emerges from the house behind you.\
      \ He sheathes a long silver blade as he addresses Mr. Sanford. “There is no sign\
      \ of Josef inside, sir.” The man eyes you suspiciously, keeping his hand on the\
      \ hilt of his sword. His posture reminds you of that of a knight, stoic and\
      \ rigidly disciplined."
    , "“I see,” Sanford replies. He closes his eyes and takes a deep breath before\
      \ continuing. “A great loss for our order, but one we anticipated. Gather the\
      \ Lodge’s belongings from inside the estate, and I will deal with the\
      \ consequences.” The knight nods and heads back inside. Carl’s cold blue eyes\
      \ turn back toward you. “As for you, I would ask you to leave this place at once.\
      \ You have done enough harm as it is.” Before you can protest, he commands the\
      \ other men nearby to escort you off the premises, and you have little choice but\
      \ to comply."
    ]

thePriceOfProgress5 :: FlavorText
thePriceOfProgress5 =
  FlavorText
    (Just "The Price of Progress 5")
    [ "You consider Mr. Sanford’s words carefully. You believe that he is telling the\
      \ truth, but you’re not sure if the ends justify his means. Before you can reply,\
      \ Josef steps forward. “I believe our new comrades understand the value of\
      \ working together,” he says politely, standing at your side. “They could have\
      \ escaped on their own, but they stayed behind to make sure members of the Lodge\
      \ were safe. Surely we are of the same mind.”"
    , "“Courage is not the same as sacrifice, Josef,” Carl Sanford intones patiently.\
      \ “Do they have the will to do what is truly right for all of humanity?” The\
      \ elderly man turns his attention back to you, his cold eyes glinting in the\
      \ moonlight. He approaches calmly, extending his hand. “Regardless, you have\
      \ experienced firsthand the danger that we all face. Join us, and we can face it\
      \ together. With our knowledge combined, we can protect Arkham from this threat.”"
    ]

thePriceOfProgress6 :: FlavorText
thePriceOfProgress6 =
  FlavorText
    (Just "The Price of Progress 6")
    [ "You consider Mr. Sanford’s words carefully. You believe that he is telling the\
      \ truth, but you’re not sure if the ends justify his means. Before you can reply,\
      \ Josef Meiger emerges from the house behind you, flanked by two guards wearing\
      \ expensive suits. One wields a long silver blade as he stands guard. The other\
      \ observes the situation vigilantly, keeping one hand on the grip of a revolver\
      \ holstered at his side. Josef recognizes you and addresses you curtly. “You have\
      \ been poking your head where you don’t belong,” he threatens, narrowing his dark\
      \ eyes."
    , "“Stay your tongue, Josef,” Sanford interjects. He approaches calmly, extending\
      \ his hand toward you. “You have experienced firsthand the danger that we all\
      \ face. Join us, and we can face it together. With our knowledge combined, we can\
      \ protect Arkham from this threat.”"
    ]

thePriceOfProgress7 :: FlavorText
thePriceOfProgress7 =
  FlavorText
    (Just "The Price of Progress 7")
    [ "Mr. Sanford scowls, returning his hand to his side and narrowing his gaze. “I\
      \ see. I cannot say I am not disappointed. I thought you would be insightful\
      \ enough to see the value in working together. But I realize now that my\
      \ confidence was misplaced.” He sighs and gazes past you, toward Josef ’s manor.\
      \ “Very well. The Lodge will handle this problem on our own. Return to your state\
      \ of ignorance. But I warn you: do not get in our way. I am not an enemy you want\
      \ to have.” Before you can protest, Sanford commands the other men nearby to\
      \ escort you off the premises, and you have little choice but to comply."
    ]

thePriceOfProgress8 :: FlavorText
thePriceOfProgress8 =
  FlavorText
    (Just "The Price of Progress 8")
    [ "You shake Mr. Sanford’s hand. His grip is cold, but strong. The other members\
      \ of the Lodge seem to relax around you. “We will do great works together, my\
      \ friends,” says Sanford. “Great works indeed.”"
    ]

thePriceOfProgress9 :: FlavorText
thePriceOfProgress9 =
  FlavorText
    (Just "The Price of Progress 9")
    [ "You shake Mr. Sanford’s hand. His grip is cold, but strong. The other members\
      \ of the Lodge seem to relax around you. “We will do great works together, my\
      \ friends,” says Sanford. “Great works indeed.” But out of the corner of your\
      \ eye, you see one of the Lodge’s enforcers gripping his weapon a little tighter."
    ]

theInnerCircle1 :: FlavorText
theInnerCircle1 =
  FlavorText
    (Just "The Inner Circle 1")
    [ "You have been brought deep into the inner sanctum, where only the\
      \ highest-ranking members of the Lodge are allowed. Mr. Sanford explains that the\
      \ Order of the Silver Twilight is far older and more important than the\
      \ public-facing Silver Twilight Lodge, and that their knowledge extends into the\
      \ realm of the arcane and the obscure. “For many decades, the Order of the Silver\
      \ Twilight has pursued knowledge that can elevate humanity. We have defended\
      \ against threats to our very existence. We have sacrificed everything for this\
      \ sacred cause. Now, one of these threats terrorizes our city, and we are the\
      \ only ones who can stop it. You know the creature I speak of.” You nod in\
      \ affirmation."
    , "“Here is what we know: it was the witches who brought this abomination upon\
      \ Arkham. We tried to stop their ritual, but unfortunately, we were unsuccessful\
      \ in binding it. Now it is loose, and we must finish what we started before the\
      \ witches are able to do the same. But first, I understand you may have some\
      \ information for us, as well. Please, hand over what you have found. It is\
      \ important that we collaborate in order to understand this situation.”"
    ]

theInnerCircle2 :: FlavorText
theInnerCircle2 =
  FlavorText
    (Just "The Inner Circle 2")
    [ "You show Mr. Sanford the evidence you’ve collected. “Hm... Yes, I see. It is as\
      \ I suspected,” he murmurs as he inspects the trinkets."
    ]

theInnerCircle3 :: FlavorText
theInnerCircle3 =
  FlavorText
    (Just "The Inner Circle 3")
    [ "You don’t trust Carl Sanford with these pieces of evidence. Who knows what he\
      \ intends to do with them? In the hands of the president of the Silver Twilight\
      \ Lodge, any one of these items could be a deadly instrument. You lie, informing\
      \ him that your investigation has so far proven fruitless. He clenches his jaw\
      \ and glares at you for a moment, his cold blue eyes staring daggers into yours.\
      \ “That is disappointing to hear. But perhaps it is my fault; I should not have\
      \ imposed such a heavy burden on a neophyte to the Order.”"
    , "Mr. Sanford rises to his feet, and the other members of the Order follow suit.\
      \ He addresses the other members of the Inner Circle: “All right. We have no more\
      \ time to spare. The ritual must be completed at once. We shall use the same site\
      \ as before—the central pillar of the Unvisited Isle, where the barrier between\
      \ this world and the next is thinnest. It is likely that the anomaly will pursue\
      \ us there, so act with caution.” Then he turns back to you. “We will require\
      \ your assistance as well, of course. The device you hold is the key to binding\
      \ the revenant. You must use it properly when the time comes. Do you understand?”\
      \ You nod in affirmation. Taking a deep breath to steel your nerves, you prepare\
      \ for the journey to the Unvisited Isle—a journey toward death itself."
    ]

theInnerCircle4 :: FlavorText
theInnerCircle4 =
  FlavorText
    (Just "The Inner Circle 4")
    [ "Carl Sanford grins and rises to his feet. “You have proven yourself a loyal\
      \ asset to our Order. I believe it is time that you ascended to our Inner\
      \ Circle.” You approach and kneel before Mr. Sanford. He spreads his arms wide\
      \ and declares, “I hereby appoint to you the rank of Knight of the Inner Circle.\
      \ Arise once more, knight, and stand tall. You are now one of the elite guardians\
      \ of humanity, keepers of the truth, and stewards of the Silver Twilight Order.”\
      \ Another member of the Order offers you a folded blue robe, and you don it\
      \ solemnly. “Now, I am sure you have many questions for me. We do not have a lot\
      \ of time to dawdle, but you deserve answers. What do you wish to know?”"
    ]

theInnerCircle5 :: FlavorText
theInnerCircle5 =
  FlavorText
    (Just "The Inner Circle 5")
    [ "Carl Sanford gives the trinkets and strange components to another member of the\
      \ Inner Circle, who retrieves them with care and brings them upstairs. “Thank you\
      \ for your assistance in this matter. We are stronger together than apart; don’t\
      \ you agree?”"
    , "Mr. Sanford rises to his feet, and the other members of the Order follow suit.\
      \ He addresses the other members of the Inner Circle: “All right. We have no more\
      \ time to spare. The ritual must be completed at once. We shall use the same site\
      \ as before—the central pillar of the Unvisited Isle, where the barrier between\
      \ this world and the next is thinnest. It is likely that the anomaly will pursue\
      \ us there, so act with caution.” Then he turns back to you. “We will require\
      \ your assistance as well, of course. The device you hold is the key to binding\
      \ the revenant. You must use it properly when the time comes. Do you understand?”\
      \ You nod in affirmation. Taking a deep breath to steel your nerves, you prepare\
      \ for the journey to the Unvisited Isle—a journey toward death itself."
    ]

theInnerCircle6 :: FlavorText
theInnerCircle6 =
  FlavorText
    (Just "The Inner Circle 6")
    [ "Mr. Sanford rises to his feet, and the other members of the Order follow suit.\
      \ He addresses the members of the Inner Circle, yourself included: “All right. We\
      \ have no more time to spare. The ritual must be completed at once. We shall use\
      \ the same site as before—the central pillar of the Unvisited Isle, where the\
      \ barrier between this world and the next is thinnest. It is likely that the\
      \ anomaly will pursue us there, so act with caution.” Then he turns directly to\
      \ you and adds, “The device you hold is the key to binding the revenant. I trust\
      \ you to use it properly when the time comes.” You and the other members of the\
      \ circle nod in affirmation. Taking a deep breath to steel your nerves, you\
      \ prepare for the journey to the Unvisited Isle—a journey toward death itself."
    ]

whatIsTheCreature :: FlavorText
whatIsTheCreature =
  FlavorText
    Nothing
    [ "“It is a powerful remnant, left behind from one who perished long ago. The\
      \ witches could only have summoned it if they had a strong connection to the soul\
      \ it once belonged to,” Mr. Sanford explains. “It could only be one person:\
      \ Keziah Mason herself.”"
    ]

whatDoYouWantWithTheCreature :: FlavorText
whatDoYouWantWithTheCreature =
  FlavorText
    Nothing
    [ "“First and foremost, we need to bind it to prevent it from doing further harm.\
      \ That is of paramount importance. Once it is bound, we may be able to learn the\
      \ secrets it possesses without endangering ourselves or others.”"
    ]

whatDoTheWitchesWantWithTheCreature :: FlavorText
whatDoTheWitchesWantWithTheCreature =
  FlavorText
    Nothing
    [ "“I am sure they wish to learn its secrets, and draw from its power,” he\
      \ theorizes, stroking his beard. “Perhaps allow one of their own to become\
      \ possessed by the spirit, and hence gain power over life and death itself.”"
    ]

didYouKnowAboutTheCreatureBeforeTheCharityGala :: FlavorText
didYouKnowAboutTheCreatureBeforeTheCharityGala =
  FlavorText
    Nothing
    [ "“In truth, yes,” Mr. Sanford admits. “But we did not know it would come to us.\
      \ After interceding in the witch’s summoning rite, we investigated the spectral\
      \ mist that was left behind for several days. We likely drew too much attention\
      \ to ourselves, and it was drawn to our presence. Not necessarily a bad thing, in\
      \ the end.”"
    ]

whereAreTheFourMissingPeopleFromTheCharityGala :: FlavorText
whereAreTheFourMissingPeopleFromTheCharityGala =
  FlavorText
    Nothing
    [ "“God only knows,” Mr. Sanford replies with a sigh. “Taken by the revenant,\
      \ perhaps. Or swallowed by the mist.” He considers this for a while, then adds,\
      \ “I suspect that if they are still alive, they cannot be far from the revenant.\
      \ For better or for worse, the anomaly seems localized around it.”"
    ]

whyAreYouLookingAtMeLikeThat :: FlavorText
whyAreYouLookingAtMeLikeThat = FlavorText Nothing ["Mr. Sanford clears his throat. “I... It is nothing,” he lies."]
