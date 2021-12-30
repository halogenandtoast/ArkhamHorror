module Arkham.Scenarios.CurtainCall.Story where

import Arkham.Prelude

import Arkham.Message

intro :: Message
intro = FlavorText
  (Just "Scenario I: Curtain Call")
  [ "You awaken with a start, as though shaken by an unseen force. You\
    \ must have slept for quite some time, for there are only a few other\
    \ patrons in the audience, and no performers on stage. The lights are\
    \ dimmed, and the stage curtains are tattered and ripped, though you\
    \ do not remember that being the case during the first act. You wait a\
    \ moment before you are sure this isn’t part of the performance. As you\
    \ wait, a foul but unrecognizable smell permeates the air. How long have\
    \ you been asleep? Shaking off your drowsiness, you walk toward one of\
    \ the seated patrons and ask for the time, but he does not respond. It is\
    \ then that you realize you are speaking to a corpse."
  ]

-- "

noResolution :: Message
noResolution = FlavorText
  (Just "No Resolution")
  [ "Once again you are startled awake, this time by\
   \ the cold, clammy fingers of a hand on your shoulder. “Are\
   \ you alright?” an elderly man asks, helping you to your feet.\
   \ Your mind is a flurry of memories. Last you remember, the\
   \ Ward Theatre had become a place of nightmares, filled with\
   \ dangerous fanatics and strange terrors. Worried, you glance\
   \ at your surroundings, only to find yourself on the rain-slicked\
   \ curb outside the theatre. Despite the events from earlier, the\
   \ city seems normal to your eyes—or at least, what passes as\
   \ normal for Arkham. The bright glare of headlights drills into\
   \ your eyes as cars pass on the street, splashing dirty rainwater\
   \ onto the sidewalk beside you. The old man wears an expression\
   \ of concern, noting the terror in your eyes. “Were you mugged?\
   \ Damn those trouble boys!” he exclaims. “Dry-gulching folk on\
   \ a night out to the the-a-tre! Not a single street those hooligans\
   \ haven’t staked a claim on, I tell you.”"
  , "You stand and walk over to the front window of the Ward\
   \ Theatre to tentatively peer through, but it is too dark to see\
   \ anything inside. The elderly man eyes you curiously for a\
   \ moment, then shrugs and continues walking. “Well, I’d best\
   \ be on my way. I would do the same, if I were you,” he says,\
   \ rounding the street corner. You quickly follow, hoping to warn\
   \ him to stay away from the theatre. But when you turn the\
   \ corner, it is not the elderly man you see, but the familiar sight\
   \ of the Stranger in his featureless, pallid mask. His unwavering\
   \ gaze bears down upon you. “Who are you?” you call out. The\
   \ Stranger does not respond, but instead turns and disappears\
   \ into the alleyway behind the theatre. You give chase, hoping for\
   \ answers, but by the time you reach the alleyway, it is empty—\
   \ save for a notice on the wall near the theatre’s employee\
   \ entrance. “Don’t be a wet blanket! Come to The King in\
   \ Yellow cast party. 8pm, at the home of Constance Dumaine,\
   \ 1452 Atlantic Avenue. Formal dress only.” You tear the notice\
   \ from the wall and take it with you, frustrated and lost."
  ]

-- "

resolution1 :: Message
resolution1 = FlavorText
  (Just "Resolution 1")
  [ "Fleeing from the theatre, you head straight to\
   \ the police station in Easttown. Bursting through the door, to the\
   \ stares of onlookers and police officers alike, you demand to see\
   \ Sheriff Engle, stressing the importance of your visit. The desk\
   \ sergeant, who is lazily working through a stack of paperwork,\
   \ shakes his head and raises a finger in silence, then points to\
   \ a nearby chair. The wait is excruciating. Every moment is an\
   \ eternity. The hands of the nearby clock crawl. You drum your\
   \ fingers on the desk. You tap your feet. You constantly peer over\
   \ your shoulder to make sure the pallid mask of the Stranger is\
   \ not watching you through the front window. Finally, the desk\
   \ sergeant puts his pen down and sits up, beckoning you. “Alright,\
   \ what’s so important now?” You are only halfway through your\
   \ explanation of the night’s events when he sighs and shakes his\
   \ head. “Look, if this is some kind of joke, it ain’t funny,” he says.\
   \ “We had officers downtown all night. Don’t try to feed me some\
   \ hooey straight to my face.” You insist, but the desk sergeant\
   \ rises to his feet and opens the door, motioning for you to leave.\
   \ He raises his voice. “What, you think we’re not busy enough or\
   \ something? Beat it!” He mumbles about “blind birds” behind\
   \ your back as he escorts you out of the station."
  , "You know what you saw earlier that night. Frustrated, you\
   \ head back to the Ward Theatre to find some kind of proof you\
   \ can take to the police. You are surprised to find that the front\
   \ door of the theatre is locked. You are about to decide whether\
   \ or not to break down the door when you see a notice on the\
   \ wall near the entrance. “Don’t be a wet blanket! Come to The\
   \ King in Yellow cast party. 8pm, at the home of Constance\
   \ Dumaine, 1452 Atlantic Avenue. Formal dress only.” You\
   \ tear the notice from the wall and fold it into your coat pocket,\
   \ hoping it will lead you to the answers you seek."
  ]

-- "

resolution2 :: Message
resolution2 = FlavorText
  (Just "Resolution 2")
  [ "You think about going to the police, but\
    \ considering the horrors in the theatre, they’re more likely to\
    \ think you’re mad than to believe your story. A small part of you\
    \ wonders if this is still some kind of prank, but that can’t be the\
    \ case—it was all too real, too terrifying to forget. Wondering\
    \ what course of action to take, you find yourself backtracking\
    \ over your escape route, returning to the theatre with cautious\
    \ steps. The bright glare of headlights drills into your eyes as cars\
    \ pass on the street, splashing dirty rainwater onto the sidewalk\
    \ beside you. Soon the Ward Theatre comes into sight, its bright\
    \ lights dominating the streets of Downtown. You expected\
    \ the theatre to be in ruins, but its exterior looks the same as it\
    \ did when you arrived to see The King in Yellow earlier that\
    \ night. You walk to the front window of the Ward Theatre and\
    \ tentatively peer through, but it is too dark to see anything inside."
  , "Just then, a chill brushes the nape of your neck, and you feel\
    \ that you are being watched. You turn, and spot a shadow\
    \ fleeing around the corner. A swarm of roaches skitters across\
    \ the sidewalk, following in the shadow’s wake. Your muscles\
    \ tense with uncertainty, and you round the corner in pursuit.\
    \ Standing unconcerned on the sidewalk is a familiar-looking\
    \ man in a featureless pallid mask."
  , "His unwavering gaze bears down upon you. “Who are you?”\
    \ you call out. The Stranger does not respond, and instead\
    \ turns and disappears into the alleyway behind the theatre.\
    \ You give chase, hoping for answers, but by the time you reach\
    \ the alleyway, it is empty—save for a notice on the wall near\
    \ the theatre’s employee entrance. “Don’t be a wet blanket!\
    \ Come to The King in Yellow cast party. 8pm, at the home\
    \ of Constance Dumaine, 1452 Atlantic Avenue. Formal dress\
    \ only.” You tear the notice from the wall and take it with you,\
    \ hoping it will lead you to the answers you seek."
  ]
