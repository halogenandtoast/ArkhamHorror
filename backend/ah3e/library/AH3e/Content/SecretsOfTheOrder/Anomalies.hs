{- | Anomaly decks. An anomaly card prints three sections and the one read is
chosen by the doom already in the space, so the same card bites harder the worse
things have got.
-}
module AH3e.Content.SecretsOfTheOrder.Anomalies (cards) where

import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill

cards :: [CardDef]
cards = fromBox SecretsOfTheOrder lostSouls

pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

anomaly :: Int -> [((Int, Maybe Int), Text, Effect)] -> CardDef
anomaly n sections =
  CardDef
    (CardCode ("lost-souls-" <> pad n))
    "Lost Souls"
    CoreSet
    1
    (AnomalyCard (AnomalyDef "Lost Souls" [(range, Encounter txt eff) | (range, txt, eff) <- sections]))

-- | Doom taken off your own space, and off anywhere in your neighborhood.
here, nearby :: Int -> Effect
here n = RemoveDoomFrom YourSpace (N n)
nearby n = RemoveDoomFrom SpaceInYourNeighborhood (N n)

-- | What the deeper bands pay out: the doom, and something to show for it.
spoils :: Int -> Effect
spoils n = Seq [here n, remnants 1]

lostSouls :: [CardDef]
lostSouls =
  [ anomaly
      1
      [
        ( (0, Just 1)
        , "A young man recoils from a looming apparition that sings a nonsensical children's rhyme (influence). If you pass, you calm the phantom long enough for the desperate man to escape; remove one doom from any space in your neighborhood. If you fail, it locks eyes with you; suffer one horror."
        , Test Influence 0 (nearby 1) (horror 1)
        )
      ,
        ( (2, Just 2)
        , "A trio of gleefully squalling spirits whirls about you, calling you by name and inviting you to join their macabre celebration (will -1). If you pass, you keep your senses and banish the phantasms; remove two doom from your space and gain one remnant. If you fail, you long to join them; suffer one horror."
        , Test Will (-1) (spoils 2) (horror 1)
        )
      ,
        ( (3, Nothing)
        , "The crashing swirl of unquiet spirits begs you for a moment's respite from their endless torment. You may become CURSED to take some of the burden they face upon yourself. If you do, the howling host grows quiet once more; remove three doom from your space and gain one remnant."
        , mayPay (CostCondition "CURSED") (spoils 3)
        )
      ]
  , anomaly
      2
      [
        ( (0, Just 1)
        , "The weeping spirit of a young woman turns her too-large face to meet yours and pleads for release (lore). If you pass, you find the old words that bind her inscribed upon a stone; remove one doom from any space in your neighborhood. If you fail, her cries grow searingly loud; suffer one damage."
        , Test Lore 0 (nearby 1) (damage 1)
        )
      ,
        ( (2, Just 2)
        , "The air grows chill and your breath catches in your throat. A shadow swoops toward you with a shriek like tearing steel (influence). If you pass, you persuade the phantom to leave you alone; remove two doom from your space and gain one remnant. If you fail, icy claws rake your arms; suffer one damage."
        , Test Influence 0 (spoils 2) (damage 1)
        )
      ,
        ( (3, Nothing)
        , "A spectral procession marches before you, singing softly of chains and servitude (will -2). If you pass, you press through the cloud of apparitions; remove three doom from your space and gain one remnant. If you fail, you join the parade for a time; suffer one horror."
        , Test Will (-2) (spoils 3) (horror 1)
        )
      ]
  , anomaly
      3
      [
        ( (0, Just 1)
        , "A desperate spirit leaps from the parapet over and over, hoping a final death will free it at last from the chains that bind it to this place. You may suffer one horror to take its icy hand and promise your help. If you do, remove one doom from any space in your neighborhood."
        , mayPay (CostHorror 1) (nearby 1)
        )
      ,
        ( (2, Just 2)
        , "The stooped apparition approaches you and smiles wickedly, its teeth and bones too long for its ragged skin. With a sneer, it offers you a chance to run (influence -1). If you pass, you calm it; remove two doom from your space and gain one remnant. If you fail, you flee; become FATIGUED."
        , Test Influence (-1) (spoils 2) fatigued
        )
      ,
        ( (3, Nothing)
        , "A chorus of unearthly voices sings a dirge of creeping pain and lingering sorrow. Your heart grows heavy and your movements still (will -1). If you pass, you quiet the song; remove three doom from your space and gain one remnant. If you fail, you fall to your knees and weep; become delayed."
        , Test Will (-1) (spoils 3) delayed
        )
      ]
  , anomaly
      4
      [
        ( (0, Just 0)
        , "A puckish spirit, driven senseless by years of torment, follows you. As it obscures your way and attempts to pilfer your belongings, you try to direct its path of chaotic disruption somewhere harmless (influence). If you pass, remove one doom from any space in your neighborhood."
        , pass Influence 0 (nearby 1)
        )
      ,
        ( (1, Just 2)
        , "The presence of a lost child seeks his mother, claimed by an outbreak of influenza nine years ago. You may become delayed to reunite the spirits at long last; remove two doom from your space, and gain one remnant. If you do not, the specter's plaintive cries follow you for hours."
        , mayPay CostDelayed (spoils 2)
        )
      ,
        ( (3, Nothing)
        , "The specters roiling before you screech and claw the air, laboring to escape from the glowing ring of a binding circle. You may press through the tormented gale and become FATIGUED to break the circle and let them rest. If you do, remove three doom from your space and gain one remnant."
        , mayPay (CostCondition "FATIGUED") (spoils 3)
        )
      ]
  , anomaly
      5
      [
        ( (0, Just 0)
        , "The spirits trapped here are confused and hurt, lashing out at anyone who blunders too close to them. You may become delayed or suffer one horror to approach them with soothing words and comfort. If you do, remove one doom from any space in your neighborhood."
        , Choose
            [ ("Become delayed", Pay CostDelayed (nearby 1))
            , ("Suffer one horror", Pay (CostHorror 1) (nearby 1))
            , ("Decline", NoEffect)
            ]
        )
      ,
        ( (1, Just 2)
        , "The hard angles, grasping talons, and mouthless face give the specter before you a truly fearsome countenance, but it doesn't move any closer to you (will). If you pass, you calmly realize that it's attempting to show you something; remove up to two doom from your space and gain one remnant."
        , pass Will 0 (spoils 2)
        )
      ,
        ( (3, Nothing)
        , "You are cornered by a swarm of phantasms, lost, angry, and bound by chains they can neither see nor comprehend (influence -2). If you pass, you calm the spirits before they fall upon you; remove three doom from your space and gain one remnant. If you fail, suffer one damage and one horror."
        , Test Influence (-2) (spoils 3) (harm 1 1)
        )
      ]
  , anomaly
      6
      [
        ( (0, Just 0)
        , "The ghost before you is little more than a cloudy silhouette, but you can feel the anger and fear radiating from it in palpable waves (will). If you pass, you calm yourself and the spectral presence; remove one doom from any space in your neighborhood."
        , pass Will 0 (nearby 1)
        )
      ,
        ( (1, Just 2)
        , "The young woman screams at a ghost you cannot see. To elude the spirit that lurks here, you'll need her to help you find it (influence -1). If you pass, she guides you both to safety; remove up to two doom from your space and gain one remnant. If you fail, your screams join hers; suffer one horror."
        , Test Influence (-1) (spoils 2) (horror 1)
        )
      ,
        ( (3, Nothing)
        , "You feel more of yourself slipping away the longer the cloud of fragmented spirits whorls around you, clawing at the edges of your soul (lore -2). If you pass, you find the secret words to drive them off; remove three doom from your space and gain one remnant. If you fail, become FATIGUED."
        , Test Lore (-2) (spoils 3) fatigued
        )
      ]
  , anomaly
      7
      [
        ( (0, Just 0)
        , "Little humanity remains in the phantom that rampages here. A loose apparition of anger, jealousy and hate, it cannot be reasoned with any longer. You may suffer one horror to drive it away and remove one doom from any space in your neighborhood."
        , mayPay (CostHorror 1) (nearby 1)
        )
      ,
        ( (1, Just 2)
        , "Fragmented souls swirl aimlessly through the air, drawn to the warmth of life and infecting the living with inarticulate rage and panic. You need only get close enough for them to feel you in order to lure them away. You may suffer up to two horror to remove an equal number of doom from your space."
        , Choose
            [ ("Suffer one horror", Pay (CostHorror 1) (here 1))
            , ("Suffer two horror", Pay (CostHorror 2) (here 2))
            , ("Decline", NoEffect)
            ]
        )
      ,
        ( (3, Nothing)
        , "A sullen tribunal of robed spirits sits in judgment, forcing you to argue your innocence of an unknown crime (influence -1). If you pass, they leave you in peace; remove three doom from your space and gain one remnant. If you fail, they pronounce their verdict and your punishment; become CURSED."
        , Test Influence (-1) (spoils 3) cursed
        )
      ]
  , anomaly
      8
      [
        ( (0, Just 0)
        , "A child mistakes you for one of the angry phantoms rampaging through the city and scrambles through a gap in a ramshackle fence (influence). If you pass, you coax the urchin out of hiding; remove one doom from any space in your neighborhood. If you fail, it takes time to reach the boy; become delayed."
        , Test Influence 0 (nearby 1) delayed
        )
      ,
        ( (1, Just 1)
        , "A maelstrom of detritus blocks the way, swept up in the raging torrent of a wild and angry phantasm. You may become FATIGUED to press through the screaming crash of flying wreckage and destroy the glyph that binds the spirit here. If you do, remove one doom from your space."
        , mayPay (CostCondition "FATIGUED") (here 1)
        )
      ,
        ( (2, Nothing)
        , "A host of gruesome spirits wavers in the air around you, blind to your presence and listening closely to something (will -1). If you pass, you release these phantoms; remove up to three doom from your space and gain one remnant. If you fail, they turn to you in unison; suffer two horror."
        , Test Will (-1) (spoils 3) (horror 2)
        )
      ]
  , anomaly
      9
      [
        ( (0, Just 0)
        , "The apparition gives you a rueful smile and says that although she knows how to help you, she is forbidden to give you aid (influence). If you pass, your riddles and games grant her a way around the strictures that compel her; remove one doom from any space in your neighborhood."
        , pass Influence 0 (nearby 1)
        )
      ,
        ( (1, Just 1)
        , "The specter of a centuries-dead surgeon traces esoteric patterns in the air with a filthy scalpel (lore). If you pass, you recognize the glyphs she draws with the blade; remove one doom from your space. If you fail, you puzzle over it for too long and are forced to flee from her knife; become delayed."
        , Test Lore 0 (here 1) delayed
        )
      ,
        ( (2, Nothing)
        , "A quartet of ravenous phantoms drum their long and gnarled fingers on their bloated and distended bellies (influence -1). If you pass, you distract them with a riddle and escape; remove up to three doom from your space and gain one remnant. If you fail, their teeth are quite sharp; suffer one damage."
        , Test Influence (-1) (spoils 3) (damage 1)
        )
      ]
  , anomaly
      10
      [
        ( (0, Just 0)
        , "A howling spirit tears through the air, swooping low to terrorize anyone who wanders into this dark and twisted alleyway (lore). If you pass, you recite a simple charm to calm emotions; remove one doom from any space in your neighborhood."
        , pass Lore 0 (nearby 1)
        )
      ,
        ( (1, Just 1)
        , "The apparitions bound here are frightened and confused, lashing out with malformed, vaporous limbs and frantic desperation (influence). If you pass, you calm the spirits and help them to leave this place; remove one doom from your space. If you fail, you are forced to flee from them."
        , pass Influence 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "Icy chains erupt from the ground to encircle your limbs and bind you in place. A rasping, rattling face appears, inches from your own (will -1). If you pass, you master your fear and escape; remove two doom from your space and gain one remnant. If you fail, the chains bite deeply; suffer two horror."
        , Test Will (-1) (spoils 2) (horror 2)
        )
      ]
  , anomaly
      11
      [
        ( (0, Just 0)
        , "The spirit pleads with you for something, but the words that crash over you are jumbled and nonsensical (influence). If you pass, you make sense of its ravings through gesture and guesswork; remove one doom from any space in your neighborhood. If you fail, it gives up, helpless and unheard."
        , pass Influence 0 (nearby 1)
        )
      ,
        ( (1, Just 1)
        , "Spectral hands rake through your flesh, as dozens of angry spirits claw at you with talons as cold as the grave (will). If you pass, you withstand the assault; remove one doom from your space. If you fail, the claws scythe through something deeper than flesh; become CURSED."
        , Test Will 0 (here 1) cursed
        )
      ,
        ( (2, Nothing)
        , "The keening phantasm traps you, infecting your mind with nightmarish visions of a world beyond your own (influence -1). If you pass, you persuade the creature to release you; remove two doom from your space and gain one remnant. If you fail, it takes hours to escape; become delayed."
        , Test Influence (-1) (spoils 2) delayed
        )
      ]
  , anomaly
      12
      [
        ( (0, Just 0)
        , "You labor to navigate gloomy shadows, plagued by a mournful, unearthly howl (will). If you pass, you find your way clear of both the darkness and the noise; remove one doom from any space in your neighborhood. If you fail, you wander too long in the maddening dark; become FATIGUED."
        , Test Will 0 (nearby 1) fatigued
        )
      ,
        ( (1, Just 1)
        , "Sickly yellow light streams from the jagged wounds that pepper the ghost's torso. When it sees you, it howls a wordless, confused plea from the remains of its tattered face (influence). If you pass, you calm the spirit and help it find much-needed rest; remove one doom from your space."
        , pass Influence 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "The crowding spirits wander in a daze, shocked and scattered by a decades-gone railway crash on the now-defunct train to Dunwich. You may become delayed to help them make sense of the accident that claimed their lives. If you do, remove two doom from your space and gain one remnant."
        , mayPay CostDelayed (spoils 2)
        )
      ]
  ]
