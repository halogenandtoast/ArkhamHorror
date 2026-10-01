-- | The neighborhood encounter decks for the tiles Under Dark Waves adds.
module AH3e.Content.UnderDarkWaves.NeighborhoodCards (cards) where

import AH3e.Content.Tiles
import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

cards :: [CardDef]
cards =
  fromBox UnderDarkWaves (centralKingsport <> innsmouthShore <> innsmouthVillage <> kingsportHarbor)

card :: NeighborhoodId -> Int -> [(Text, Text, Effect)] -> CardDef
card nid n encounters =
  CardDef
    { code = CardCode (coerce nid <> "-" <> T.justifyRight 2 '0' (tshow n))
    , name = (tile nid).name <> " " <> tshow n <> "/8"
    , expansion = CoreSet
    , copies = 1
    , kind =
        NeighborhoodCard
          nid
          (Map.fromList [(spaceIdFor place, Encounter txt eff) | (place, txt, eff) <- encounters])
    }

centralKingsport :: [CardDef]
centralKingsport =
  map
    (uncurry (card "central-kingsport"))
    [
      ( 1
      ,
        [
          ( "Congregational Hospital"
          , "Between coughing fits, a gaunt young woman tells you that her end has been foretold. She shows you an ornate tarot card and begs you to take it. Gain DEATH. When you mention it to a doctor, he says that you look unwell and may need help. You may spend $1 for you or an ally to recover two health."
          , Seq [named "DEATH", mayPay (SpendMoney 1) (health 2)]
          )
        ,
          ( "Hall School"
          , "Exhausted, you must have dozed off in a storage room. In your dream, you witness a young student performing an arcane ritual. Gain one spell. The girl seems to sense your presence and looks around for you. You try to avoid being discovered (lore). If you fail, she spots your dream self and shouts at you with an ancient voice; suffer one horror."
          , Seq [spell, Test Lore 0 NoEffect (horror 1)]
          )
        ,
          ( "Neil's Curiosity Shop"
          , "\"Quite a story,\" Neil smiles. \"Hard to believe. Do you have any proof? Because I know people—people with money...\" His voice trails off briefly. \"I'll offer you a trade.\" His eyes race across the various oddities in his shop. \"Something fair and useful. What do you say?\" You may spend one remnant to gain one curio."
          , mayPay (SpendRemnants 1) curioItem
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Congregational Hospital"
          , "On the wall, you see a photo of the church that stood here once. As you examine the people in the image, their eyes study you as well (will). If you pass, a look of serenity settles over the long-dead congregants; become BLESSED. If you fail, a black stain covers the picture; become TAINTED."
          , Test Will 0 blessed tainted
          )
        ,
          ( "Hall School"
          , "\"We have another visitor today,\" Victoria Bryant says, \"You two might have a lot in common.\" She introduces you to a stranger who is sorting through heavy boxes of books in the basement. Gain one ally. Having already searched here, you suggest moving on (influence). If you fail, you strain yourself sorting the massive tomes again; suffer one damage."
          , Seq [ally, Test Influence 0 NoEffect (damage 1)]
          )
        ,
          ( "Neil's Curiosity Shop"
          , "A sign says that these items are being sold at a discount due to some slight water damage. Next to the sign is an article about cargo recovered from a ship that sank mysteriously. You may buy one curio from the display for half price (rounded up). If you buy anything, you suspect you know what caused the ship to sink; become TAINTED."
          , BuyFromDisplay (Just "Curio") HalfPrice (Just 1) tainted
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Congregational Hospital"
          , "You follow a whisper into the basement and then into a sub-basement. As the tunnels grow darker and more winding, you fear that you won't find the way back. You may gain a DARK PACT to keep going and recover all of your health and sanity. If you do, you wake up safe and sound in a hospital bed."
          , mayPay (CostCondition "DARK PACT") (Custom "recover-all")
          )
        ,
          ( "Hall School"
          , "Principal Miles tells you that someone has been seen sneaking around the campus at night. You reassure him that you will catch the prowler (observation). If you pass, you discover that the figure watching in the night is sympathetic to your cause and has been patrolling the campus to keep the students safe; gain one ally."
          , pass Observation 0 ally
          )
        ,
          ( "Neil's Curiosity Shop"
          , "You question why a collection of dull stone chips is so expensive. The shopkeep laughs and tells you that it's the story that goes with them that makes them so valuable. \"If you decide to buy the stones,\" he smiles, \"I'll give you the story of how they came into my possession for no extra charge.\" You may spend $1 to gain one remnant."
          , mayPay (SpendMoney 1) (remnants 1)
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Congregational Hospital"
          , "One of the orderlies offers to steal simple medicines from the hospital for a price. He also tells you that he can get his hands on the remains of a strange creature that resides in the morgue. You may spend $1 to pay the man. If you do, you gain one remnant and you or an ally may recover two health."
          , mayPay (SpendMoney 1) (Seq [remnants 1, health 2])
          )
        ,
          ( "Hall School"
          , "A young woman introduces herself to you as Asenath, and tells you about the strange books she saw here as a student. You ask if she still has any of them (influence). If you pass, she gives you a knowing smile and pulls a book from her bag; gain EBEN HALL'S JOURNAL. If you fail, her face suddenly seems older and crueler; become TAINTED."
          , Test Influence 0 (named "EBEN HALL'S JOURNAL") tainted
          )
        ,
          ( "Neil's Curiosity Shop"
          , "Neil keeps a box of unsalable odds and ends by the door, including a shell carved with peculiar symbols. Gain one remnant. As you examine it, you are overcome by a vision of the ocean and it takes you a moment to recover your senses (will). If you fail, you find several hours have passed in that moment; become delayed."
          , Seq [remnants 1, Test Will 0 NoEffect delayed]
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Congregational Hospital"
          , "You seize a chance to sneak into the hospital's files and steal a photograph of the monstrous cadaver found in the basement. Gain one remnant. You hear a nurse approaching and must either sneak out while you can or bribe her to continue your search. You may spend $1 to gain an additional remnant."
          , Seq [remnants 1, mayPay (SpendMoney 1) (remnants 1)]
          )
        ,
          ( "Hall School"
          , "At a school rally, the students sing a peculiar melody. Dean Bryant tells you the song is a school tradition that predates her arrival at the school more than twenty years ago. As their voices echo throughout the hall you try to decipher the lyrics (observation). If you pass, the music inspires and invigorates you; become BLESSED."
          , pass Observation 0 blessed
          )
        ,
          ( "Neil's Curiosity Shop"
          , "The floor of the shop is littered with crates, each containing a random assortment of wares. Neil has always been a friend and you know he would appreciate you buying as much as you can. It would spare him having to sort through everything. You may buy any number of curios from the display."
          , buyAny "Curio"
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Congregational Hospital"
          , "You or an ally may recover two health. After your treatment, a wild-eyed woman in the lobby calls, \"If any of you seen what I did, you'd know I need my pills!\" You may spend $1 to pay for her medicine in exchange for a story and a dark leather pouch filled with small bones. If you do, gain one remnant."
          , Seq [health 2, mayPay (SpendMoney 1) (remnants 1)]
          )
        ,
          ( "Hall School"
          , "This yearbook once belonged to Asenath Waite. Inside you find an elaborate map to some unnamed hidden treasure. Following all the cryptic hints and sketches will take time, but it might be worth it. You may become delayed to search for her hidden library. If you do, you find journals filled with occult notes and diagrams; gain two spells."
          , mayPay CostDelayed (Seq [spell, spell])
          )
        ,
          ( "Neil's Curiosity Shop"
          , "As you walk through the aisles you hear a faint bell ringing and strange eyes look back from mirrors all around you. A whispered voice says, \"Leave an offering. Receive a gift.\" You may spend one remnant to placate the presence and gain one curio. If you do not, the voice grows louder and more demanding; suffer one horror."
          , MayPay (SpendRemnants 1) curioItem (horror 1)
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Congregational Hospital"
          , "The doctor is hesitant to provide you with any treatment. Fearing that your condition may be worse than it appears, she would rather keep you at the hospital for observation before making a diagnosis. You may become delayed for you or an ally to recover three health."
          , mayPay CostDelayed (health 3)
          )
        ,
          ( "Hall School"
          , "As Principal Miles leads you on a tour of the campus, he mentions a strange series of runes carved into the floor of an unused part of the building. You ask him to show you these symbols (influence). If you pass, he leads you down a dark hall until you reach an abandoned classroom and the arcane writing within; gain one spell."
          , pass Influence 0 spell
          )
        ,
          ( "Neil's Curiosity Shop"
          , "As you wander the store, you ignore the wares for sale. You find yourself more interested in watching Neil assess the value of incoming stock. You strike up a conversation about his skills and knowledge. Flattered, Neil offers you a deal. For a fee, he will make his expertise available to you. You may spend $2 to gain an EYE FOR APPRAISAL."
          , mayPay (SpendMoney 2) (named "EYE FOR APPRAISAL")
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Congregational Hospital"
          , "While the doctor expertly treats your wounds, you tell her about the dangers you have faced. You or an ally may recover two health. Seeing that you will continue to be at risk, she offers to treat your more long-standing health issues. You may spend $1 for you or an ally to recover two additional health."
          , Seq [health 2, mayPay (SpendMoney 1) (health 2)]
          )
        ,
          ( "Hall School"
          , "A group of students are clustered around an unfamiliar figure. You strike up a conversation hoping to impress the stranger (influence). If you pass, you persuade the teenage girls to introduce you; gain one ally. If you fail, you quickly grow concerned that everyone is laughing at you behind your back; suffer one horror."
          , Test Influence 0 ally (horror 1)
          )
        ,
          ( "Neil's Curiosity Shop"
          , "A crying child is struggling to describe a symbol, but unfortunately the proprietor doesn't recognize it. The young boy collapses, whispering, \"no one will believe me,\" to himself over and over. You may spend one remnant to give the boy a necklace bearing the mark he spoke of. If you do, his joy comforts you; become BLESSED."
          , mayPay (SpendRemnants 1) blessed
          )
        ]
      )
    ]

innsmouthShore :: [CardDef]
innsmouthShore =
  map
    (uncurry (card "innsmouth-shore"))
    [
      ( 1
      ,
        [
          ( "Falcon Point"
          , "A fisherman has injured his arm and offers to pay you to help gather his nets. Gain $2. The net is snagged on something under the water, making it nearly impossible to pull in his haul (strength). If you pass, you find an object in among the pale, gasping fish; gain one curio."
          , Seq [money 2, pass Strength 0 curioItem]
          )
        ,
          ( "Gilman House"
          , "For all your misgivings about Innsmouth, you do get a good night's sleep. You or an ally may recover two sanity. In the morning you speak to another guest who couldn't sleep at all. You may spend one remnant to prove to the other guest that you are familiar with supernatural threats. If you do, gain one ally."
          , Seq [sanity 2, mayPay (SpendRemnants 1) ally]
          )
        ,
          ( "Marsh Refinery"
          , "You try to sneak past the workers to get inside the refinery's office (observation). If you pass, you find historic documents about the early Innsmouth citizens; gain one remnant. If you fail, the workers spot you and begin to croak like monstrous toads, and you feel a shadow settle over you; become TAINTED."
          , Test Observation 0 (remnants 1) tainted
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Falcon Point"
          , -- the curio is gained outright; the test only risks the damage
            "Something beneath the water catches your eye. You reach down and discover something useful. Gain one curio. You greedily search the sea floor for more lost treasures (strength). If you fail, you exhaust yourself plunging into the cold water repeatedly; suffer one damage."
          , Seq [curioItem, Test Strength 0 NoEffect (damage 1)]
          )
        ,
          ( "Gilman House"
          , "You ask Othera Gilman, the owner, about the history of the hotel and Innsmouth (influence). If you pass, the two of you have a long and pleasant conversation and she is convinced you would be a good addition to the hotel's staff; become a HOTEL PORTER. If you fail, she eyes you warily and clams up until you find your own way to the door."
          , pass Influence 0 (named "HOTEL PORTER")
          )
        ,
          ( "Marsh Refinery"
          , "Near the refinery, you speak to some of the local children about what happens inside (influence). If you pass, they show you a ledger that one of them stole; gain one remnant. If you fail, they don't want to talk with you, claiming that the last person who talked to strangers was thrown into the ocean to feed the fish; suffer one horror."
          , Test Influence 0 (remnants 1) (horror 1)
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Falcon Point"
          , "Something comes over you and you find yourself walking into the ocean. When you reach a point where you can barely keep your head above water, you abruptly dive to the bottom and resurface with pieces of gold. Gain $3 before you struggle back to shore and suffer two damage."
          , Seq [money 3, damage 2]
          )
        ,
          ( "Gilman House"
          , "A family is arguing about whether or not to stay in Innsmouth. You believe that if you gave them proof of the dangers in this village, you might save their lives. You may spend one remnant to prove beyond doubt the threats that lurk here. If you do, the family leaves immediately, thanking you for warning them; become BLESSED."
          , mayPay (SpendRemnants 1) blessed
          )
        ,
          ( "Marsh Refinery"
          , "In the middle of the night you sneak into the refinery, flashlight in hand, and search for the source of the gold coming into Innsmouth (observation). If you pass, you discover a large map on the wall of the office with several locations circled; spawn one clue. If you fail, you slink away before the brutish guards find you here."
          , pass Observation 0 SpawnOneClue
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Falcon Point"
          , "In the faint moonlight, you see a boat full of silhouetted figures that seems to be following you as you walk down the shore. When you start to run, they cry out and hurl something that embeds itself in your leg. Suffer one damage and gain the HARPOON."
          , Seq [damage 1, named "HARPOON"]
          )
        ,
          ( "Gilman House"
          , "Othera Gilman starts to tell you stories about Innsmouth, but she is distracted by a souvenir you have acquired during your travels. You may spend one remnant to coax Othera to finish telling her stories. If you do, you find the mundane anecdotes to be strangely comforting; you or an ally may recover three sanity."
          , mayPay (SpendRemnants 1) (sanity 3)
          )
        ,
          ( "Marsh Refinery"
          , "You pose as a wealthy importer, interested in business with Innsmouth's refineries (influence). If you pass, Jacob Marsh takes you at your word and describes his business dealings throughout the area; spawn one clue. If you fail, Jacob does not believe you but promises that he will not forget your face; suffer one horror."
          , Test Influence 0 SpawnOneClue (horror 1)
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Falcon Point"
          , "A circle of robed figures chant abominations toward the sea. Repulsed by their ritual, you rush forward to interrupt these acolytes (strength). If you pass, they flee into the night and you feel reassured, certain that something monstrous has been averted; become BLESSED."
          , pass Strength 0 blessed
          )
        ,
          ( "Gilman House"
          , "The people on the bus to Arkham are waiting for their driver. You talk to the passengers in hopes of finding someone who can help you (influence). If you pass, you make a new friend; gain one ally. If you fail, Joe Sargent returns while you are speaking to the passengers and he glares at you with large fish-like eyes; become TAINTED."
          , Test Influence 0 ally tainted
          )
        ,
          ( "Marsh Refinery"
          , -- the remnant is gained outright; the test only risks the damage
            "A drainage pipe leads from the refinery to a ditch nearby. In the mud there, you discover a necklace with strange golden decorations. Gain one remnant. You root through the water in the ditch for more (observation). If you fail, you spend too much time in the toxic water and it burns your skin; suffer one damage."
          , Seq [remnants 1, Test Observation 0 NoEffect (damage 1)]
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Falcon Point"
          , "A storm blows in suddenly and you scramble to find safety from the icy rain and cutting winds. You're nearly frozen when you finally find a cave. In the cavern, you find an abandoned object that might prove useful. Suffer two damage from the weather and gain one curio."
          , Seq [damage 2, curioItem]
          )
        ,
          ( "Gilman House"
          , "Othera Gilman says that there are no rooms available, but you suspect she might just be inhospitable to strangers (influence). If you pass, she hands you the keys to one of the nicest rooms in the place, with all the comforts of home; you or an ally may recover three sanity. If you fail, she curtly directs you to the bus station."
          , pass Influence 0 (sanity 3)
          )
        ,
          ( "Marsh Refinery"
          , "A truck sits outside the refinery waiting to drop off raw materials. You cautiously approach and try to sneak into the back (observation). If you pass, you are shocked to find the truck is empty except for a small piece of gold jewelry shaped into a strange squid-like image; gain one remnant."
          , pass Observation 0 (remnants 1)
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Falcon Point"
          , "A slouching Innsmouth citizen is shambling toward the choppy water, and you try to stop her (strength). If you pass, her family arrives and rewards you with pieces of gold; gain $4. If you fail, she disappears beneath the waves and you feel a strange desire to follow her into the depths; become TAINTED."
          , Test Strength 0 (money 4) tainted
          )
        ,
          ( "Gilman House"
          , "You catch some sleep as soon as you arrive at the hotel and wake up thoroughly refreshed. You or an ally may recover two sanity. None of the food at the hotel seems appetizing to you, but you might be able to get a local restaurant to deliver to you (influence). If you pass, the dinner is a warm comfort; you or an ally may recover two sanity."
          , Seq [sanity 2, pass Influence 0 (sanity 2)]
          )
        ,
          ( "Marsh Refinery"
          , "In the middle of the night, dark shambling figures approach the refinery. One of them sees you and recites a blasphemous chant. Gain TWISTED FLESH. You run from the creatures and look for a place to hide (observation). If you fail, they rip and tear your flesh with their talons; suffer two damage."
          , Seq [named "TWISTED FLESH", Test Observation 0 NoEffect (damage 2)]
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Falcon Point"
          , "An old sea chest has washed up on the shore. When you examine it, you find the old padlock that holds the lid shut is thoroughly rusted. You try to pry the thing open (strength). If you pass, the padlock falls away and the water-tight chest's contents are revealed; gain one curio."
          , pass Strength 0 curioItem
          )
        ,
          ( "Gilman House"
          , "A nervous stranger sits outside waiting for Joe Sargent's bus. When you strike up a conversation, you learn that this person is running away after losing a friend to the Deep Ones. You may spend one remnant to prove that you have successfully faced that threat before. If you do, you inspire this stranger to keep fighting; gain one ally."
          , mayPay (SpendRemnants 1) ally
          )
        ,
          ( "Marsh Refinery"
          , "A woman in a blue robe cautiously approaches the refinery. When you greet her, she introduces herself as an acolyte of Nodens, called to observe this place. The two of you share a conversation about her faith in the old powers (influence). If you pass, the acolyte rests a hand on your shoulder and grants you her god's protection; become BLESSED."
          , pass Influence 0 blessed
          )
        ]
      )
    ]

innsmouthVillage :: [CardDef]
innsmouthVillage =
  map
    (uncurry (card "innsmouth-village"))
    [
      ( 1
      ,
        [
          ( "Esoteric Order of Dagon"
          , "You walk through the empty temple and step up to an altar adorned with strange runes (lore). If you pass, you press down on key symbols in order and unlock a secret cabinet; gain the GOLDEN CROWN. If you fail, touching the altar makes you feel nauseated and unclean; suffer one horror."
          , Test Lore 0 (named "GOLDEN CROWN") (horror 1)
          )
        ,
          ( "First National Grocery"
          , "Brian Burnham offers you a sample of fruit and vegetables that just arrived on the truck from Boston. You or an ally may recover two health. \"Pretty good, right?\" says Brian. \"Better buy it while it's fresh. Something in the air here makes the produce turn quickly.\" You may pay $2 for you or an ally to recover two additional health."
          , Seq [health 2, mayPay (SpendMoney 2) (health 2)]
          )
        ,
          ( "Innsmouth Jail"
          , "Chief Constable Martin left a key piece of evidence unattended on his desk. Gain one common item. Before you can leave the room, Martin walks in and demands that you explain what you are doing (will). If you fail, he whispers some abominable ancient words into your ear; become TAINTED."
          , Seq [commonItem, Test Will 0 NoEffect tainted]
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Esoteric Order of Dagon"
          , "The congregation reads some sort of invocation aloud. To blend in, you sit at a pew and begin to read along with the others. You aren't sure what you are saying but the words sicken you. Suffer two horror. When the invocation is complete, you feel a strange aura surround you. Become BLESSED."
          , Seq [horror 2, blessed]
          )
        ,
          ( "First National Grocery"
          , "\"Are you feeling okay?\" A woman shopping at the grocery identifies herself as a doctor from Boston. \"If you need any medical care, I'd be happy to help.\" Based on her fine suit, you imagine her services are expensive. You may pay $2 for you or an ally to recover three health."
          , mayPay (SpendMoney 2) (health 3)
          )
        ,
          ( "Innsmouth Jail"
          , "Constable Ropes is not terribly bright, and he has a reputation for cruelty. You try to trick him into letting you look through the collection of things he has collected from prisoners (influence). If you pass, Ropes takes a liking to you and invites you to help yourself; gain one common item."
          , pass Influence 0 commonItem
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Esoteric Order of Dagon"
          , "Robert Marsh calls you by name. \"Come forward, child, and receive your gift.\" Gain one curio. Marsh begins to chant and gesture, part of some blasphemous ritual you hope you can defend against (lore). If you fail, you feel as if you are floating in a fetid pool; become TAINTED."
          , Seq [curioItem, Test Lore 0 NoEffect tainted]
          )
        ,
          ( "First National Grocery"
          , "A group of women are collecting donations to help support widows and orphans who lost family members in the Great War. You may spend $1 to donate to their charitable efforts. If you do, you feel a warm glow knowing you've performed a truly benevolent act; become BLESSED."
          , mayPay (SpendMoney 1) blessed
          )
        ,
          ( "Innsmouth Jail"
          , "\"Hey! I need you to do me a favor!\" A handcuffed prisoner covertly passes you a stolen item. Gain one common item. When Constable Ropes comes back in, the prisoner accuses you of being a thief. You protest that you are innocent (influence). If you fail, Ropes makes you spend some time in jail; become delayed."
          , Seq [commonItem, Test Influence 0 NoEffect delayed]
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Esoteric Order of Dagon"
          , "No one is in the temple tonight and yet you feel an unnatural presence. This lurking phantom surrounds you, its invisible tendrils wrapping around you and strangling your thoughts. Suffer one horror and gain one spell as this ethereal horror alters your mind."
          , Seq [horror 1, spell]
          )
        ,
          ( "First National Grocery"
          , "It's an unusually sunny day and you take a small break sitting on the bench outside the grocery. You or an ally may recover one health. Young Brian Burnham pokes his head out the store's front door and offers to sell you some iced tea. You may spend $1 for you or an ally to recover two health."
          , Seq [health 1, mayPay (SpendMoney 1) (health 2)]
          )
        ,
          ( "Innsmouth Jail"
          , -- "as through" is the card's own typo, kept as printed
            "\"A toast to the Tattered King!\" comes a voice from one of the cells. A ragged looking prisoner holds up a tin cup filled with fetid water, and speaks to you as through you were both guests at a fancy gala. You indulge him in his delusion (influence). If you pass, he bows elegantly and passes you a ragged tarot card; gain the FOUR OF CUPS."
          , pass Influence 0 (named "FOUR OF CUPS")
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Esoteric Order of Dagon"
          , "A few older believers gather in a circle and speak in an ancient language, and you memorize what they say. Gain one spell. When they are done, you see nightmarish visions of underwater horrors and cast a protective ward (lore). If you fail, the hallucinations overwhelm you; suffer two horror."
          , Seq [spell, Test Lore 0 NoEffect (horror 2)]
          )
        ,
          ( "First National Grocery"
          , "A friendly neighbor offers to buy your groceries, and you gratefully accept, picking out a few necessities. You or an ally may recover two health. When your benefactor goes to pay, she spills change all across the floor. You offer to help her find all the loose coins (observation). If you pass, you find one more coin than she dropped; gain the LUCKY COIN."
          , Seq [health 2, pass Observation 0 (named "LUCKY COIN")]
          )
        ,
          ( "Innsmouth Jail"
          , "You look through the photographs from a violent crime scene that Chief Constable Martin has left on his desk. The pictures depict scenes of horror and mayhem, and you have a hard time looking at them (will). If you pass, you eventually find the photo that establishes the presence of dark influences; gain one remnant."
          , pass Will 0 (remnants 1)
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Esoteric Order of Dagon"
          , "You make your way downstairs where you find a large stone well. As you look down into the black water, a voice fills your thoughts, calling to you with promises to reward faithful service with arcane might. You may pledge your loyalty and gain a DARK PACT to gain two spells."
          , mayPay (CostCondition "DARK PACT") (Seq [spell, spell])
          )
        ,
          ( "First National Grocery"
          , "You arrive at the grocery just as Brian Burnham is opening up. He greets you with a broad, genuine smile. \"There's a sale on biscuits! People love 'em and they're gonna go fast.\" You may spend $1 to take advantage of this sale. If you do, you have to agree that those are some really good biscuits; you or an ally may recover two health."
          , mayPay (SpendMoney 1) (health 2)
          )
        ,
          ( "Innsmouth Jail"
          , "Constable Ropes wants you to testify against a priest visiting from Boston who is accused of murder. He badgers you for hours, trying to coerce you to support his lie (will). If you pass, the grateful priest is released due to lack of evidence and he thanks you profusely with a gift and a prayer; gain one common item and become BLESSED."
          , pass Will 0 (Seq [commonItem, blessed])
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Esoteric Order of Dagon"
          , "An elderly Innsmouth resident approaches and places a gift in your hands. Her appearance is so repulsively fish-like that you can barely stand to look at her, and she struggles to speak through her over-sized mouth. Suffer one horror as a result of her inhuman nature and gain one curio."
          , Seq [horror 1, curioItem]
          )
        ,
          ( "First National Grocery"
          , "At the front counter is an opened bottle of tonic with a label promising to \"Fix what ails ya!\" You or an ally may recover two health. It looks as if this was bottled here in Innsmouth. You get a bad feeling about the ingredients and try to figure out what went into this bottle (observation). If you fail, the dreadful smell of the tonic is unfamiliar; suffer one horror."
          , Seq [health 2, Test Observation 0 NoEffect (horror 1)]
          )
        ,
          ( "Innsmouth Jail"
          , "Chief Constable Martin orders you to check on a prisoner who isn't responding. You enter the man's cell and find his cold body twisted into impossible angles (will). If you pass, you find an object the man concealed in his mattress; gain one common item. If you fail, the man's cruel and brutal end turns your stomach; suffer two horror."
          , Test Will 0 commonItem (horror 2)
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Esoteric Order of Dagon"
          , "Murals along the wall depict strange iconic figures in underwater vistas. You try to interpret these images (lore). If you pass, you see that the depiction of the night sky above the water is particularly meaningful and find a hidden panel in the wall here behind the stars; gain one curio."
          , pass Lore 0 curioItem
          )
        ,
          ( "First National Grocery"
          , "The grocery is busy today and poor Brian Burnham is overwhelmed. When you come to the front of the line, he accidentally overcharges you and is too harried to redo his math. An impatient person behind you snaps, \"Pay the bill or step aside!\" You may spend $2 for you or an ally to recover two health."
          , mayPay (SpendMoney 2) (health 2)
          )
        ,
          ( "Innsmouth Jail"
          , "Constable Ropes jabs a truncheon into your shoulder and tells you that police protection is not free (influence). If you pass, he walks away counting his small and petty bribe, unaware that he left his filing cabinet unlocked; gain one remnant. If you fail, he tells you that you do not deserve his protection; suffer one damage."
          , Test Influence 0 (remnants 1) (damage 1)
          )
        ]
      )
    ]

kingsportHarbor :: [CardDef]
kingsportHarbor =
  map
    (uncurry (card "kingsport-harbor"))
    [
      ( 1
      ,
        [
          ( "North Point Lighthouse"
          , "When you try to enter the lighthouse you instead step into some sort of other world, navigating through an illogical maze that tends to lead you back outside (lore). If you pass, you successfully find your way to the perfectly mundane interior and find a container filled with fuel; gain the KEROSENE."
          , pass Lore 0 (named "KEROSENE")
          )
        ,
          ( "The Rope and Anchor"
          , "A haggard-looking ship's captain sits in a dark corner. She offers to tell you the story of how she came to be hexed. You may become TAINTED and listen to her tale. If you do, you learn about otherworldly powers among other important life lessons; spawn one clue and you or an ally may recover two sanity."
          , mayPay (CostCondition "TAINTED") (Seq [SpawnOneClue, sanity 2])
          )
        ,
          ( "St. Erasmus's Home"
          , "Sadly, one of the residents has passed away and no one has stepped forward to settle his affairs. You volunteer to sort through the man's debts if you can (influence). If you pass, you find something among the man's belongings that might prove useful to you; gain one common item."
          , pass Influence 0 commonItem
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "North Point Lighthouse"
          , "You look out from the lighthouse across the water on a moonless night (observation). If you pass, you spot a boat helplessly adrift and call for help, saving several lives; become BLESSED. If you fail, the wreckage of a lost boat washes ashore along with the bodies of the crew that you failed to save; suffer two horror."
          , Test Observation 0 blessed (horror 2)
          )
        ,
          ( "The Rope and Anchor"
          , "Jonas Rigg welcomes you to his bar and offers your first drink on the house. \"The Feds have a tendency to overlook this old place.\" You or an ally may recover two sanity. If you bother to buy a round for the sailors next to you, they will regale you with tales that delight you for hours. You may spend $1 for you or an ally to recover two additional sanity."
          , Seq [sanity 2, mayPay (SpendMoney 1) (sanity 2)]
          )
        ,
          ( "St. Erasmus's Home"
          , "Granny Orne is paying a visit and spies one of the souvenirs you have picked up during your struggle against the forces that threaten our reality. \"I ain't seen a carving like that since I was a little girl. Oh, that takes me back. You wouldn't be interested in selling that now, would you?\" You may spend one remnant to gain $3."
          , mayPay (SpendRemnants 1) (money 3)
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "North Point Lighthouse"
          , "From the fog, a crew of ghostly sailors surrounds you. You know it is possible to banish such spirits by means of the proper chant (lore). If you pass, their unearthly influence is undone; remove one doom from any space. If you fail, the ethereal sailors mark your hand with a black circle; become TAINTED."
          , Test Lore 0 (RemoveDoomFrom AnySpaceWithDoom (N 1)) tainted
          )
        ,
          ( "The Rope and Anchor"
          , "A desperate man shambles up to you. He insists that he has something important to tell you, but can't gather his wits enough to say what it is that is so vital. You may spend $1 to buy him a drink and calm him down. If you do, he shares vital information about the unearthly threats that haunt Arkham; you may spawn or research one clue."
          , mayPay (SpendMoney 1) (Custom "spawn-or-research-clue")
          )
        ,
          ( "St. Erasmus's Home"
          , "You've heard that a resident left behind a sea chest filled with gold but no one will touch it for fear of dark magic. You ask Granny Orne about the chest (influence). If you pass, she gives you a key and says you are welcome to anything you find; gain $4. If you fail, she offers no help and the rumors prove true; become TAINTED."
          , Test Influence 0 (money 4) tainted
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "North Point Lighthouse"
          , "For some reason, the lighthouse has gone dark. You search the mechanism from top to bottom, looking for what might be the problem (observation). If you pass, you repair the issue and the light shines once again, filling the night with inspirational light; remove one doom from any space."
          , pass Observation 0 (RemoveDoomFrom AnySpaceWithDoom (N 1))
          )
        ,
          ( "The Rope and Anchor"
          , "Jonas Rigg is in the grip of a terrible fever. Unless you step in to take his place, the establishment will remain closed until he recovers. You may become delayed to stay here, serving food and discreet drinks to the sailors. If you do, Jonas's customers gratefully offer a prayer to the sea to keep you safe; become BLESSED."
          , mayPay CostDelayed blessed
          )
        ,
          ( "St. Erasmus's Home"
          , "You overhear Granny Orne describing a particular sigil to one of the old sailors, but he thinks she's just making up nonsense. You search your pockets for something to back up her old tales. Granny is delighted. \"If you have it, I'll trade you for it!\" she declares. You may spend one remnant to gain one common item."
          , mayPay (SpendRemnants 1) commonItem
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "North Point Lighthouse"
          , "For a moment you see a colleague lost in the thick fog. You cry out to guide your friend, who vanishes into the mist. Another investigator may move one space. You can no longer see the lighthouse (observation). If you fail, you suddenly catch yourself before tumbling over a cliff's edge; suffer one horror."
          , Seq [Custom "another-investigator-moves", Test Observation 0 NoEffect (horror 1)]
          )
        ,
          ( "The Rope and Anchor"
          , "A well-dressed man with red gloves gestures for you to join him. His soothing voice lulls you into a restorative trance. You or an ally may recover two sanity. You try to force yourself back to full consciousness before the man leaves (will). If you pass, the man pauses before departing to hand you a piece of paper; gain the STRANGER'S CONTRACT."
          , Seq [sanity 2, pass Will 0 (named "STRANGER'S CONTRACT")]
          )
        ,
          ( "St. Erasmus's Home"
          , "Granny Orne sits alone with a sad expression. You try to cheer her up, hoping she will recall better times (influence). If you pass, her mood improves and she offers you a gift; gain one common item. If you fail, her stories center around some of the terrible tragedies that have beset Kingsport over the years; suffer two horror."
          , Test Influence 0 commonItem (horror 2)
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "North Point Lighthouse"
          , "Basil Elton tells you strange tales of traveling to other worlds, but you struggle to find meaning in the nonsense he spouts (lore). If you pass, you decipher the means to transport a person from one place to another through doorways to the realm of dreams; another investigator may move one space."
          , pass Lore 0 (Custom "another-investigator-moves")
          )
        ,
          ( "The Rope and Anchor"
          , "An old timer wants to make a bet that his stories of ghosts and ghouls are more terrifying than anything you've ever experienced. Looking at him, you suspect that you might lose the wager, but hearing his stories might be worth the money. You may spend $1. If you do, his stories prove to be very illuminating; spawn one clue."
          , mayPay (SpendMoney 1) SpawnOneClue
          )
        ,
          ( "St. Erasmus's Home"
          , "An old man calls to you in an unrecognizable language. He pulls you forward and presses a parcel into your hands. You feel a deep sense of dread about accepting this gift. You may gain a DARK PACT to gain two common items. If you do not, the man spits on you and invokes a dark god's name; become TAINTED."
          , MayPay (CostCondition "DARK PACT") (Seq [commonItem, commonItem]) tainted
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "North Point Lighthouse"
          , "Basil points to the beach, but it's not clear at first what he's indicating. Each time the beam of light from the beacon sweeps that direction, you scan the shoreline (observation). If you pass, you see a nightgaunt, which Basil claims can be used temporarily as a flying steed; any investigator may move one space."
          , pass Observation 0 (Custom "any-investigator-moves")
          )
        ,
          ( "The Rope and Anchor"
          , "The sailors are taking up a collection for the families of men who have been lost at sea. You know that if you were to make a contribution, the thought of helping widows and orphans would ease your mind. You may spend $1 for you or an ally to recover two sanity."
          , mayPay (SpendMoney 1) (sanity 2)
          )
        ,
          ( "St. Erasmus's Home"
          , "Various members of the community volunteer at the home. When you talk to them about the dangers you face, they seem skeptical. You may spend one remnant to prove that what you say is true. If you do, they promise you to spread the word about your important work; gain a FRIEND OF A FRIEND."
          , mayPay (SpendRemnants 1) (named "FRIEND OF A FRIEND")
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "North Point Lighthouse"
          , "Gazing out over the moonlit water, you try to call forth the White Ship (lore). If you pass, the craft and its crew stand ready for their next journey between worlds; any investigator may move one space. If you fail, you know you were heard by something but no ship arrives; become TAINTED."
          , Test Lore 0 (Custom "any-investigator-moves") tainted
          )
        ,
          ( "The Rope and Anchor"
          , "A lot of the regulars are out to sea, so you and your companions have the staff's undivided attention. \"The more the merrier!\" roars Jonas Rigg. The evening passes happily and for just a few coins, you all feel thoroughly refreshed. You may spend $1 for each ally and investigator in your space to recover one sanity. (You are an investigator in your space.)"
          , Custom "round-of-drinks"
          )
        ,
          ( "St. Erasmus's Home"
          , "A crackling radio plays quiet music in an otherwise silent room occupied by old sailors lost in their memories. Hoping to lighten the spirits here, you begin loudly making small talk and sharing playful anecdotes (influence). If you pass, the residents laugh and embrace you as a welcome ray of light in their lives; become BLESSED."
          , pass Influence 0 blessed
          )
        ]
      )
    ]
