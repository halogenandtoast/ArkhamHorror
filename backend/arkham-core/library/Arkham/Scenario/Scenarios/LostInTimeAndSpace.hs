module Arkham.Scenario.Scenarios.LostInTimeAndSpace
  ( LostInTimeAndSpace(..)
  , lostInTimeAndSpace
  ) where

import Arkham.Prelude

import Arkham.Act.Types ( Field (..) )
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as AS
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Types ( Field (..) )
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait hiding ( Cultist )

newtype LostInTimeAndSpace = LostInTimeAndSpace ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTimeAndSpace :: Difficulty -> LostInTimeAndSpace
lostInTimeAndSpace difficulty = scenario
  LostInTimeAndSpace
  "02311"
  "Lost in Time and Space"
  difficulty
  [ ".              .                  .                  tearThroughSpace2 tearThroughSpace2    tearThroughSpace1    tearThroughSpace1  .                 .                 ."
  , ".              .                  .                  tearThroughSpace2 tearThroughSpace2    tearThroughSpace1    tearThroughSpace1  .                 .                 ."
  , ".              tearThroughSpace3  tearThroughSpace3  .                 .                    .                    .                  tearThroughSpace4 tearThroughSpace4 ."
  , ".              tearThroughSpace3  tearThroughSpace3  .                 .                    .                    .                  tearThroughSpace4 tearThroughSpace4 ."
  , "endlessBridge2 endlessBridge2     endlessBridge1     endlessBridge1    .                    .                    prismaticCascade1  prismaticCascade1 prismaticCascade2 prismaticCascade2"
  , "endlessBridge2 endlessBridge2     endlessBridge1     endlessBridge1    .                    .                    prismaticCascade1  prismaticCascade1 prismaticCascade2 prismaticCascade2"
  , ".              dimensionalDoorway dimensionalDoorway .                 anotherDimension     anotherDimension     .                  stepsOfYhagharl   stepsOfYhagharl   ."
  , ".              dimensionalDoorway dimensionalDoorway .                 anotherDimension     anotherDimension     .                  stepsOfYhagharl   stepsOfYhagharl   ."
  , ".              .                  .                  .                 tearThroughTime      tearThroughTime      .                  .                 .                 ."
  , ".              .                  .                  .                 tearThroughTime      tearThroughTime      .                  .                 .                 ."
  , ".              .                  .                  .                 theEdgeOfTheUniverse theEdgeOfTheUniverse .                  .                 .                 ."
  , ".              .                  .                  .                 theEdgeOfTheUniverse theEdgeOfTheUniverse .                  .                 .                 ."
  ]

instance HasTokenValue LostInTimeAndSpace where
  getTokenValue iid tokenFace (LostInTimeAndSpace attrs) = case tokenFace of
    Skull -> do
      extradimensionalCount <- selectCount $ LocationWithTrait Extradimensional
      pure $ TokenValue
        Skull
        (NegativeModifier $ if isEasyStandard attrs
          then min extradimensionalCount 5
          else extradimensionalCount
        )
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ toTokenValue attrs Tablet 3 5
    ElderThing -> do
      mlid <- field InvestigatorLocation iid
      shroud <- case mlid of
        Nothing -> pure 0
        Just lid -> field LocationShroud lid
      pure $ toTokenValue attrs ElderThing shroud (shroud * 2)
    otherFace -> getTokenValue iid otherFace attrs

standaloneTokens :: [TokenFace]
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

investigatorDefeat :: (Monad m, HasGame m) => ScenarioAttrs -> m [Message]
investigatorDefeat a = do
  leadInvestigatorId <- getLeadInvestigatorId
  defeatedInvestigatorIds <- selectList DefeatedInvestigator
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
      <> [ InvestigatorKilled (toSource a) iid
         | iid <- defeatedInvestigatorIds
         ]

instance RunMessage LostInTimeAndSpace where
  runMessage msg s@(LostInTimeAndSpace attrs) = case msg of
    SetTokensForScenario -> do
      standalone <- getIsStandalone
      s <$ if standalone then push (SetTokens standaloneTokens) else pure ()
    Setup -> do
      investigatorIds <- getInvestigatorIds
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.yogSothoth]
        [ EncounterSet.LostInTimeAndSpace
        , EncounterSet.Sorcery
        , EncounterSet.TheBeyond
        , EncounterSet.HideousAbominations
        , EncounterSet.AgentsOfYogSothoth
        ]

      anotherDimension <- EncounterCard
        <$> genEncounterCard Locations.anotherDimension

      let anotherDimensionId = LocationId $ toCardId anotherDimension

      pushAll
        [ story investigatorIds lostInTimeAndSpaceIntro
        , SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , PlaceLocation anotherDimension
        , RevealLocation Nothing anotherDimensionId
        , MoveAllTo (toSource attrs) anotherDimensionId
        ]

      setAsideCards <- traverse
        genCard
        [ Locations.theEdgeOfTheUniverse
        , Locations.tearThroughTime
        , Enemies.yogSothoth
        ]

      LostInTimeAndSpace <$> runMessage
        msg
        (attrs
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL
          . at 1
          ?~ [ Acts.outOfThisWorld
             , Acts.intoTheBeyond
             , Acts.closeTheRift
             , Acts.findingANewWay
             ]
          )
        & (agendaStackL
          . at 1
          ?~ [ Agendas.allIsOne
             , Agendas.pastPresentAndFuture
             , Agendas.breakingThrough
             , Agendas.theEndOfAllThings
             ]
          )
        )
    After (PassedSkillTest iid _ _ (TokenTarget token) _ _) ->
      s <$ case (isHardExpert attrs, tokenFace token) of
        (True, Cultist) ->
          push
            (DiscardEncounterUntilFirst
              (toSource attrs)
              (CardWithType LocationType)
            )
        (_, Tablet) -> do
          mYogSothothId <- selectOne (EnemyWithTitle "Yog-Sothoth")
          case mYogSothothId of
            Nothing -> pure ()
            Just eid -> push (EnemyAttack iid eid DamageAny RegularAttack)
        _ -> pure ()
    After (FailedSkillTest iid _ _ (TokenTarget token) _ _) ->
      s <$ case tokenFace token of
        Cultist -> push
          (DiscardEncounterUntilFirst
            (ProxySource (toSource attrs) (InvestigatorSource iid))
            (CardWithType LocationType)
          )
        Tablet -> do
          mYogSothothId <- selectOne (EnemyWithTitle "Yog-Sothoth")
          case mYogSothothId of
            Nothing -> pure ()
            Just eid -> push (EnemyAttack iid eid DamageAny RegularAttack)
        _ -> pure ()
    RequestedEncounterCard (ProxySource source (InvestigatorSource iid)) mcard
      | isSource attrs source -> s <$ case mcard of
        Nothing -> pure ()
        Just card -> pushAll
          [ PlaceLocation (EncounterCard card)
          , MoveTo (toSource attrs) iid (LocationId $ toCardId card)
          ]
    ScenarioResolution NoResolution -> do
      actId <- selectJust AnyAct
      step <- fieldMap ActSequence (AS.unActStep . AS.actStep) actId
      push (ScenarioResolution . Resolution $ if step == 4 then 2 else 4)
      pure . LostInTimeAndSpace $ attrs & inResolutionL .~ True
    ScenarioResolution (Resolution 1) -> do
      msgs <- investigatorDefeat attrs
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      xp <- getXp
      pushAll
        (msgs
        <> [ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   (Just "Resolution 1")
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
        <> [ GainXP iid (n + 5) | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
        )
      pure . LostInTimeAndSpace $ attrs & inResolutionL .~ True
    ScenarioResolution (Resolution 2) -> do
      msgs <- investigatorDefeat attrs
      leadInvestigatorId <- getLeadInvestigatorId
      pushAll
        (msgs
        <> [ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   (Just "Resolution 2")
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
           , EndOfGame Nothing
           ]
        )
      pure . LostInTimeAndSpace $ attrs & inResolutionL .~ True
    ScenarioResolution (Resolution 3) -> do
      msgs <- investigatorDefeat attrs
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      pushAll
        (msgs
        <> [ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   (Just "Resolution 3")
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
        <> [ InvestigatorKilled (toSource attrs) iid
           | iid <- investigatorIds
           ]
        <> [EndOfGame Nothing]
        )
      pure . LostInTimeAndSpace $ attrs & inResolutionL .~ True
    ScenarioResolution (Resolution 4) -> do
      msgs <- investigatorDefeat attrs
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      pushAll
        (msgs
        <> [ chooseOne
             leadInvestigatorId
             [ Run
                 [ Continue "Continue"
                 , FlavorText
                   (Just "Resolution 4")
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
        <> [EndOfGame Nothing]
        )
      pure . LostInTimeAndSpace $ attrs & inResolutionL .~ True
    _ -> LostInTimeAndSpace <$> runMessage msg attrs
