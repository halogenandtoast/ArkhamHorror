module Arkham.Types.Scenario.Scenarios.LostInTimeAndSpace
  ( LostInTimeAndSpace(..)
  , lostInTimeAndSpace
  ) where

import Arkham.Prelude

import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.EnemyMatcher
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait hiding (Cultist)

newtype LostInTimeAndSpace = LostInTimeAndSpace ScenarioAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTimeAndSpace :: Difficulty -> LostInTimeAndSpace
lostInTimeAndSpace difficulty = LostInTimeAndSpace $ baseAttrs
  "02311"
  "Lost in Time and Space"
  ["02312", "02313", "02314", "02315"]
  ["02316", "02317", "02318", "02319"]
  difficulty

instance HasRecord LostInTimeAndSpace where
  hasRecord _ = pure False
  hasRecordSet _ = pure []
  hasRecordCount _ = pure 0

instance
  ( HasSet LocationId env [Trait]
  , HasTokenValue env InvestigatorId
  , HasCount Shroud env LocationId
  , HasId LocationId env InvestigatorId
  )
  => HasTokenValue env LostInTimeAndSpace where
  getTokenValue (LostInTimeAndSpace attrs) iid = \case
    Skull -> do
      extradimensionalCount <- length <$> getSet @LocationId [Extradimensional]
      pure $ TokenValue
        Skull
        (NegativeModifier $ if isEasyStandard attrs
          then min extradimensionalCount 5
          else extradimensionalCount
        )
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ toTokenValue attrs Tablet 3 5
    ElderThing -> do
      lid <- getId @LocationId iid
      shroud <- unShroud <$> getCount lid
      pure $ toTokenValue attrs ElderThing shroud (shroud * 2)
    otherFace -> getTokenValue attrs iid otherFace

standaloneTokens :: [Token]
standaloneTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusThree
  , MinusFour
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

lostInTimeAndSpaceIntro :: Message
lostInTimeAndSpaceIntro = FlavorText
  (Just "Scenario VII: Lost in Time and Space")
  [ "Passing through the gate is unlike anything you’ve\
    \ ever experienced. You feel your body twisting and\
    \ distorting, churning through realities as the gate\
    \ pulls you to its ultimate destination—a place\
    \ beyond the scope of human imagination."
  , "Suddenly, all is quiet and the chaos of the\
    \ journey is replaced with a sense of solitude and dread. You are in an\
    \ unfathomable place, vast beyond your ability to reason and utterly\
    \ alien besides. The landscape is surreal and strange, the architecture\
    \ impossible. You are so far from home that home has become a\
    \ threadbare dream you can barely recall. Even should you find a way out\
    \ of this awful place, you may never be the same again."
  ]

investigatorDefeat
  :: ( MonadReader env m
     , HasSet DefeatedInvestigatorId env ()
     , HasId LeadInvestigatorId env ()
     )
  => m [Message]
investigatorDefeat = do
  leadInvestigatorId <- getLeadInvestigatorId
  defeatedInvestigatorIds <- map unDefeatedInvestigatorId <$> getSetList ()
  if null defeatedInvestigatorIds
    then pure []
    else
      pure
      $ [ chooseOne
            leadInvestigatorId
            [ Run
                [ Continue "Continue"
                , FlavorText
                  Nothing
                  [ "Where did you come from? Why are\
                    \ you here? Are you dreaming, or is this place real? Now that\
                    \ you think about it, haven’t you been here before? Or perhaps\
                    \ you’ve been here all along. Now you remember. This is your\
                    \ real home. The path you now walk is but one ledge, with many\
                    \ more below. You only have to fall and you will be where you\
                    \ belong. One more step…"
                  ]
                ]
            ]
        ]
      <> [ InvestigatorKilled iid | iid <- defeatedInvestigatorIds ]

instance
  ( HasSet InvestigatorId env ()
  , HasSet DefeatedInvestigatorId env ()
  , ScenarioAttrsRunner env
  , HasStep env ActStep
  , HasId (Maybe EnemyId) env EnemyMatcher
  , HasId (Maybe LocationId) env LocationMatcher
  , HasCount XPCount env ()
  )
  => RunMessage env LostInTimeAndSpace where
  runMessage msg s@(LostInTimeAndSpace attrs) = case msg of
    SetTokensForScenario -> do
      standalone <- getIsStandalone
      s <$ if standalone
        then unshiftMessage (SetTokens standaloneTokens)
        else pure ()
    Setup -> do
      investigatorIds <- getInvestigatorIds
      encounterDeck <- buildEncounterDeckExcluding
        ["02323"]
        [ EncounterSet.LostInTimeAndSpace
        , EncounterSet.Sorcery
        , EncounterSet.TheBeyond
        , EncounterSet.HideousAbominations
        , EncounterSet.AgentsOfYogSothoth
        ]
      anotherDimensionId <- getRandom
      unshiftMessages
        [ story investigatorIds lostInTimeAndSpaceIntro
        , SetEncounterDeck encounterDeck
        , AddAgenda "02312"
        , AddAct "02316"
        , PlaceLocation "02320" anotherDimensionId
        , RevealLocation Nothing anotherDimensionId
        , MoveAllTo anotherDimensionId
        ]

      let
        locations' = mapFromList $ map
          (second pure . toFst (getLocationName . lookupLocationStub))
          [ "02320"
          , "02321"
          , "02322"
          , "02324"
          , "02325"
          , "02326"
          , "02327"
          , "02328"
          ]
      LostInTimeAndSpace <$> runMessage
        msg
        (attrs
        & (locationsL .~ locations')
        & (setAsideLocationsL .~ ["02321", "02322"])
        )
    After (PassedSkillTest iid _ _ (DrawnTokenTarget token) _ _) ->
      s <$ case (isHardExpert attrs, drawnTokenFace token) of
        (True, Cultist) -> unshiftMessage
          (DiscardEncounterUntilFirst
            (toSource attrs)
            (CardMatchByType (LocationType, mempty))
          )
        (_, Tablet) -> do
          mYogSothothId <- getId (EnemyWithTitle "Yog-Sothoth")
          case mYogSothothId of
            Nothing -> pure ()
            Just eid -> unshiftMessage (EnemyAttack iid eid)
        _ -> pure ()
    After (FailedSkillTest iid _ _ (DrawnTokenTarget token) _ _) ->
      s <$ case drawnTokenFace token of
        Cultist -> unshiftMessage
          (DiscardEncounterUntilFirst
            (ProxySource (toSource attrs) (InvestigatorSource iid))
            (CardMatchByType (LocationType, mempty))
          )
        Tablet -> do
          mYogSothothId <- getId (EnemyWithTitle "Yog-Sothoth")
          case mYogSothothId of
            Nothing -> pure ()
            Just eid -> unshiftMessage (EnemyAttack iid eid)
        _ -> pure ()
    RequestedEncounterCard (ProxySource source (InvestigatorSource iid)) mcard
      | isSource attrs source -> s <$ case mcard of
        Nothing -> pure ()
        Just card -> unshiftMessages
          [ PlaceLocation (toCardCode card) (LocationId $ toCardId card)
          , MoveTo iid (LocationId $ toCardId card)
          ]
    ScenarioResolution NoResolution -> do
      step <- unActStep <$> getStep
      s <$ unshiftMessage
        (ScenarioResolution . Resolution $ if step == 4 then 2 else 4)
    ScenarioResolution (Resolution 1) -> do
      msgs <- investigatorDefeat
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      s <$ unshiftMessages
        (msgs
        <> [ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   Nothing
                   [ "Lying on your back in a patch of wet grass, you\
                   \ find yourself staring longingly at the night sky. Somehow, you\
                   \ are once again atop Sentinel Hill, unable to recall exactly how\
                   \ you got here. You are mesmerized by the night sky. Seconds\
                   \ become minutes, and minutes become hours. Eventually,\
                   \ you are found and lifted to your feet by a group of Dunwich\
                   \ citizens. “What happened? What are ya doin’ here?” they ask\
                   \ you, frightened but curious. You can’t seem to find the right\
                   \ words to describe the events that occurred beyond the gate…if\
                   \ they ever truly occurred. There doesn’t appear to be any trace\
                   \ of Seth Bishop, of the creatures you fought earlier, or of the\
                   \ phantasmal and otherworldly gate. But every time you sleep,\
                   \ you dream—and when you dream, it all comes rushing back."
                   ]
                 ]
             ]
           , Record TheInvestigatorsClosedTheTearInReality
           ]
        <> [ SufferTrauma iid 2 2 | iid <- investigatorIds ]
        <> [ GainXP iid (xp + 5) | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    ScenarioResolution (Resolution 2) -> do
      msgs <- investigatorDefeat
      leadInvestigatorId <- getLeadInvestigatorId
      s <$ unshiftMessages
        (msgs
        <> [ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   Nothing
                   [ "Several of the villagers from Dunwich heard\
                     \ the ruckus on Sentinel Hill and went to investigate. What they\
                     \ found there answered none of their questions."
                   , "“What d’you think happened?” a frightened Curtis Whateley\
                     \ asks as they examine the hilltop. The other villagers shake\
                     \ their heads, unable to say. “Last thing I saw, the sky’d open’d\
                     \ up an’ there was a bright flash,” one of them says, looking up at\
                     \ the starry sky from the top of Sentinel Hill."
                   , "“Those Arkham fellas ain’t nowhere to be seen,” Curtis adds,\
                     \ looking down at the cracked stone altar. “Not even of a hint of\
                     \ ‘em. Mr. Bishop and his pals, now, that’s a diff’rent story.” The\
                     \ young man motions toward several corpses on the ground."
                   , "“Think we should get ahold of those coppers outta Aylesbury?”"
                   , "“Why, so they can laugh at us again?” Curtis spits. “They ain’t\
                     \ never gonna believe us ‘bout all this anyway.” He glances at\
                     \ the stone altar, fidgeting nervously. “Better t’ forget about the\
                     \ whole thing. C’mon, let’s bury the dead and get outta here.”"
                   ]
                 ]
             ]
           , EndOfGame
           ]
        )
    ScenarioResolution (Resolution 3) -> do
      msgs <- investigatorDefeat
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      s <$ unshiftMessages
        (msgs
        <> [ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   Nothing
                   [ "The creature erupts in a cosmic fury of sound,\
                     \ color, and distorted space, hurling you back and away from it.\
                     \ You watch in horror as one of its arms tears through the fabric\
                     \ of the world, and its amorphous shape funnels through the\
                     \ rift, pulling the threads of the world along with it. You try to\
                     \ cling to something, but you are inexorably sucked into the rift.\
                     \ You feel as if your body is stretching and your mind is being\
                     \ crushed. Then, everything goes black."
                   ]
                 ]
             ]
           , Record YogSothothHasFledToAnotherDimension
           ]
        <> [ InvestigatorKilled iid | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    ScenarioResolution (Resolution 4) -> do
      msgs <- investigatorDefeat
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      s <$ unshiftMessages
        (msgs
        <> [ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   Nothing
                   [ "The sorcerers from Dunwich, seeking arcane\
                     \ power from beyond this realm, have accomplished what\
                     \ Wilbur and Old Whateley could not. Through blood sacrifice\
                     \ and indescribable experiments, the dark power the sorcerers\
                     \ sought is now within their reach. However, they will never get\
                     \ the chance to truly wield this power. In beseeching Wilbur’s\
                     \ father for knowledge, they have drawn the creature forth\
                     \ from its extradimensional realm. Yog-Sothoth emerges from\
                     \ the open rift above Sentinel Hill, blotting out the sky and\
                     \ enveloping the world. Now it has come to Earth, and it rules\
                     \ where humanity once tread."
                   ]
                 ]
             ]
           , Record
             YogSothothToreApartTheBarrierBetweenWorldsAndBecameOneWithAllReality
           ]
        <> [ DrivenInsane iid | iid <- investigatorIds ]
        <> [EndOfGame]
        )
    _ -> LostInTimeAndSpace <$> runMessage msg attrs
