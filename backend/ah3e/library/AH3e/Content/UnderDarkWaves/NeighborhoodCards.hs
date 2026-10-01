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
cards = fromBox UnderDarkWaves centralKingsport

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
