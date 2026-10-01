{- | Anomaly decks. An anomaly card prints three sections and the one read is
chosen by the doom already in the space, so the same card bites harder the worse
things have got.
-}
module AH3e.Content.UnderDarkWaves.Anomalies (cards) where

import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill

cards :: [CardDef]
cards = fromBox UnderDarkWaves visionsOfTheMoon

pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

anomaly :: Int -> [((Int, Maybe Int), Text, Effect)] -> CardDef
anomaly n sections =
  CardDef
    (CardCode ("visions-of-the-moon-" <> pad n))
    "Visions of the Moon"
    CoreSet
    1
    ( AnomalyCard
        (AnomalyDef "Visions of the Moon" [(range, Encounter txt eff) | (range, txt, eff) <- sections])
    )

-- | Doom taken off your own space, and off anywhere in your neighborhood.
here, nearby :: Int -> Effect
here n = RemoveDoomFrom YourSpace (N n)
nearby n = RemoveDoomFrom SpaceInYourNeighborhood (N n)

remnant :: Effect
remnant = remnants 1

-- | What the deeper bands almost always pay out: the doom, and a gem to show for it.
spoils :: Int -> Effect
spoils n = Seq [here n, remnant]

visionsOfTheMoon :: [CardDef]
visionsOfTheMoon =
  [ anomaly
      1
      [
        ( (0, Just 1)
        , "The Men of Leng load glowing spheres onto a black galley bound for the Dreamlands. You try to disrupt the crew to stop the shipment (will). If you pass, you escape with some of the orbs; remove one doom from any space in your neighborhood. If you fail, you are loaded onto the ship; become delayed."
        , Test Will 0 (nearby 1) delayed
        )
      ,
        ( (2, Just 2)
        , "You watch the helmsman steer the black galley to the Dreamlands, and try to control the ship when he leaves you unattended (observation -1). If you pass, you sail the ship safely back home; remove two doom from your space and gain a remnant. If you fail, the ship crashes back to Earth; suffer two damage."
        , Test Observation (-1) (spoils 2) (damage 2)
        )
      ,
        ( (3, Nothing)
        , "The Men of Leng drag you below the black galley's deck, but you urge them to rebel against their masters (influence -2). If you pass, they mutiny and give you a small red gem; remove three doom from your space and gain a remnant. If you fail, their moon-beast lord pounces; suffer three damage."
        , Test Influence (-2) (spoils 3) (damage 3)
        )
      ]
  , anomaly
      2
      [
        ( (0, Just 1)
        , "A traveler from the City on the Moon offers you a sample of pazu. You take a small drink of the sweet nectar (will). If you pass, your mind is awakened to the realm of dreams; remove one doom from any space in your neighborhood. If you fail, you are plagued by nightmarish visions; suffer one horror."
        , Test Will 0 (nearby 1) (horror 1)
        )
      ,
        ( (2, Just 2)
        , "You sneak aboard a black galley from the Dreamlands, where you find crates of red gems. You could throw them overboard, but it would be a strenuous effort. You may suffer two damage to do so, keeping one stone for yourself; remove two doom from your space and gain a remnant."
        , mayPay (CostDamage 2) (spoils 2)
        )
      ,
        ( (3, Nothing)
        , "A disembodied voice emerges from behind you, offering you a glass of pazu. You may gain a DARK PACT by drinking from the glass in front of you. If you do, you feel the power of a god coursing through you; remove three doom from your space and gain a spell."
        , mayPay (CostCondition "DARK PACT") (Seq [here 3, spell])
        )
      ]
  , anomaly
      3
      [
        ( (0, Just 1)
        , "The White Ship has sailed into view. You ask the crew to help you recover the souls of people abducted by the monstrous corsairs and their allies (influence). If you pass, the ship sets out to track down the black galleys; remove one doom from any space in your neighborhood."
        , pass Influence 0 (nearby 1)
        )
      ,
        ( (2, Just 2)
        , "A black galley takes you to the Dreamlands where you steal a red jewel and then dive overboard to escape (will). If you pass, you hide beneath the silver waves until the White Ship rescues you; remove two doom from your space and gain a remnant. If you fail, you are recaptured; become delayed."
        , Test Will 0 (spoils 2) delayed
        )
      ,
        ( (3, Nothing)
        , "In the Dreamlands, the crew of the White Ship welcomes you aboard. You receive a compass to help guide the ship to your home (observation -1). If you pass, the crew repairs the damage done by their enemies and lets you keep the compass; remove three doom from your space and gain a remnant."
        , pass Observation (-1) (spoils 3)
        )
      ]
  , anomaly
      4
      [
        ( (0, Just 0)
        , "The Men of Leng are here to buy captured spirits. You tell the robed merchants you can make them a better offer if they disregard their deal with the cultists (influence). If you pass, you convince the otherworldly beings to leave; remove one doom from any space in your neighborhood."
        , pass Influence 0 (nearby 1)
        )
      ,
        ( (1, Just 2)
        , "A cultist in a bone-white mask negotiates with a Man of Leng. You believe unmasking the merchant would spoil the deal. You may become TAINTED to steal the strange creature's cowl. If you do, the negotiation is over; remove up to two doom from your space and gain a remnant."
        , mayPay (CostCondition "TAINTED") (spoils 2)
        )
      ,
        ( (3, Nothing)
        , "The Men of Leng give you a blood-red gem. You look deep into the stone and see a horrific fate for Arkham (will -1). If you pass, you accept the gift and the merchants depart; remove three doom from your space and gain a remnant. If you fail, you see your blood is in the gem; become CURSED."
        , Test Will (-1) (spoils 3) cursed
        )
      ]
  , anomaly
      5
      [
        ( (0, Just 0)
        , "An enormous bat-like creature swoops down out of the air to grab you in its talons; suffer one horror. At the last minute, two nightgaunt servants of Nodens drive the shantak away and save you from its grasp. Remove one doom from any space in your neighborhood."
        , Seq [horror 1, nearby 1]
        )
      ,
        ( (1, Just 2)
        , "A shantak corners you. You beg Nodens to save you (influence -1). If you pass, a rain of seashells drives the beast away; remove up to two doom from your space and gain a remnant. If you fail, a disembodied, many-throated voice mocks your cries for help; suffer two horror."
        , Test Influence (-1) (spoils 2) (horror 2)
        )
      ,
        ( (3, Nothing)
        , "One of Nyarlathotep's shantaks has flown you high into the night sky and dropped you (will -2). If you pass, you are conscious when Nodens catches you in his chariot and he rewards your courage with a rune-carved conch shell; remove three doom from your space and gain a remnant."
        , pass Will (-2) (spoils 3)
        )
      ]
  , anomaly
      6
      [
        ( (0, Just 0)
        , "You stand on a strange landscape with a crystal tower on the horizon. Looking up, you see the Earth where the moon should be. You cry out to the night sky for help (influence). If you pass, winged nightgaunts descend to aid your journey; remove one doom from any space in your neighborhood."
        , pass Influence 0 (nearby 1)
        )
      ,
        ( (1, Just 2)
        , "You step through a doorway that leads to vast crystal halls. A distant gong calls to you (will -1). If you pass, you return and seal the door that led here; remove one doom from your space. If you fail, an alien presence fills your head with visions of your own death; suffer one damage and one horror."
        , Test Will (-1) (here 1) (harm 1 1)
        )
      ,
        ( (3, Nothing)
        , "Servants of evil drag you into a grand crystal hall to stand before Nyarlathotep itself. The Dark Messenger wishes to strike a bargain and offers you a blood-red crystal to seal the pact. You may become TAINTED to remove three doom from your space and gain a remnant."
        , mayPay (CostCondition "TAINTED") (spoils 3)
        )
      ]
  , anomaly
      7
      [
        ( (0, Just 0)
        , "Masked figures are waiting for the Men of Leng to trade white globes for their scarlet gems. You may spend a remnant to give them one of your own gems. If you do, you acquire the orbs yourself and release the captured spirit energy; remove one doom from any space in your neighborhood."
        , mayPay (SpendRemnants 1) (nearby 1)
        )
      ,
        ( (1, Just 2)
        , "The Men of Leng will abandon their terrible cargo if you agree to go below the deck of the black galley they sailed from the Dreamlands. You may become TAINTED to receive a red gem from the moon-beast within the ship. If you do, remove two doom from your space and gain a remnant."
        , mayPay (CostCondition "TAINTED") (spoils 2)
        )
      ,
        ( (3, Nothing)
        , "The Men of Leng give you a ruby-red gem and explain in a whisper the horrific way the gems are formed (will -2). If you pass, you steel yourself and tell the strangers to depart; remove three doom from your space and gain a remnant. If you fail, a fever overtakes you; become TAINTED."
        , Test Will (-2) (spoils 3) tainted
        )
      ]
  , anomaly
      8
      [
        ( (0, Just 0)
        , "From deep in the shadows, a long snaking wisp reaches for the moon and begins to tear the sky open (will). If you pass, you shine your torch into the shadow, revealing empty air; remove one doom from any space in your neighborhood. If you fail, strange energies envelop you; become TAINTED."
        , Test Will 0 (nearby 1) tainted
        )
      ,
        ( (1, Just 1)
        , "The cultists begin their chant to weaken the walls between worlds. You loudly describe the horrors that await if their ritual is successful, knowing that if at least one of them wavers, the magic will fail (influence). If you pass, their spell falters; remove one doom from your space."
        , pass Influence 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "A fleet of black galleys sails through the air. You scramble to find a hiding place (observation -1). If you pass, the ships soon disappear; remove two doom from your space and gain a remnant. If you fail, Men of Leng on the ships shout at you in a strange language; become CURSED."
        , Test Observation (-1) (spoils 2) cursed
        )
      ]
  , anomaly
      9
      [
        ( (0, Just 0)
        , "Something has transported you to the forests on the moon. Shantak-birds fly overhead and moon-beasts lurk among the trees (will). If you pass, you wait patiently until you see the White Ship overhead, coming to your rescue; remove one doom from any space in your neighborhood."
        , pass Will 0 (nearby 1)
        )
      ,
        ( (1, Just 1)
        , "A stowaway brought to the moon by the black galleys has disappeared into the lunar forests. You try to track him down before some vicious creature does (observation). If you pass, you recover the lost one and return them safely to Arkham; remove one doom from your space."
        , pass Observation 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "Shantaks circle overhead, eager to take you to the Dreamlands. You cry out to Nodens to save you (influence -1). If you pass, nightgaunts rend the creatures to pieces and you retrieve one of the shantak's talons for a souvenir; remove up to three doom from your space and gain a remnant."
        , pass Influence (-1) (spoils 3)
        )
      ]
  , anomaly
      10
      [
        ( (0, Just 0)
        , "You step through the doorway that leads to the Dreamlands. It will take days to locate the White Ship, but time flows differently there. You may become delayed to find the ship and secure their assistance. If you do, remove one doom from any space in your neighborhood."
        , mayPay CostDelayed (nearby 1)
        )
      ,
        ( (1, Just 1)
        , "The crew of the White Ship will guide you through the Dreamlands, visiting some of the most disturbing locations (will). If you pass, your journeys allow you to undo the damage done to reality; remove one doom from your space. If you fail, the voyage marks your soul; become CURSED."
        , Test Will 0 (here 1) cursed
        )
      ,
        ( (2, Nothing)
        , "The crew of the White Ship finds you lost in the Dreamlands. You borrow an astrolabe to guide them to your world so they can help you seal the passage between worlds (observation -1). If you pass, remove up to three doom from your space and gain a remnant. If you fail, suffer three horror."
        , Test Observation (-1) (spoils 3) (horror 3)
        )
      ]
  , anomaly
      11
      [
        ( (0, Just 0)
        , "Moon-beasts chase you across the lunar surface! A cavern leads down into a dark labyrinth where you believe you can hide. On the shores of a glassy black underground lake, you call out to the silence for aid (influence). If you pass, remove one doom from any space in your neighborhood."
        , pass Influence 0 (nearby 1)
        )
      ,
        ( (1, Just 1)
        , "Parched and weary, you stand before a massive lake beneath the moon's surface. Repulsive as it may be, you take a deep drink of the oily frothing waters (will). If you pass, you harness the power of the lake; remove one doom from your space. If you fail, you feel wholly unclean; become TAINTED."
        , Test Will 0 (here 1) tainted
        )
      ,
        ( (2, Nothing)
        , "A great beast, hunted by the corsairs, lurks in a cavern deep within the moon. You may become TAINTED to bargain with the creature. If you do, he stares at you with eyeless sockets and gives you the bones of a moon-beast; remove two doom from your space and gain a remnant."
        , mayPay (CostCondition "TAINTED") (spoils 2)
        )
      ]
  , anomaly
      12
      [
        ( (0, Just 0)
        , "The Men of Leng emerge from the portal, searching for victims to bring back to the Dreamlands. You keep yourself hidden (observation). If you pass, these robed strangers abandon their efforts; remove one doom from any space in your neighborhood. If you fail, you have to fight to free yourself; suffer one damage."
        , Test Observation 0 (nearby 1) (damage 1)
        )
      ,
        ( (1, Just 1)
        , "Entranced dockhands load heavy crates onto a black-sailed galley, and you try to rouse them against the hooded corsairs (influence). If you pass, they wake from their stupor and flee; remove one doom from your space. If you fail, the spell falls over you as well, and you join the work; become TAINTED."
        , Test Influence 0 (here 1) tainted
        )
      ,
        ( (2, Nothing)
        , "A moon-beast has emerged from a milky-white portal (will -1). If you pass, you lure the beast and lead it back to where it came from; remove up to three doom from your space and gain a remnant. If you fail, the creature corners you; suffer one damage and one horror."
        , Test Will (-1) (spoils 3) (harm 1 1)
        )
      ]
  ]
