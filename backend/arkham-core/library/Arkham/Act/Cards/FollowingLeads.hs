module Arkham.Act.Cards.FollowingLeads (
  FollowingLeads (..),
  followingLeads,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Scenarios.MurderAtTheExcelsiorHotel.FlavorText
import Arkham.Trait (Trait (CrimeScene, Cultist, Guest, Innocent, Lead, Staff))
import Arkham.Treachery.Cards qualified as Treacheries

newtype FollowingLeads = FollowingLeads ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

followingLeads :: ActCard FollowingLeads
followingLeads = act (2, A) FollowingLeads Cards.followingLeads Nothing

instance HasAbilities FollowingLeads where
  getAbilities (FollowingLeads x) =
    [ restrictedAbility x 1 (AssetCount 2 $ AssetWithTrait Lead <> AssetWithClues (atLeast 1))
        $ Objective
        $ ReactionAbility (RoundEnds #when) Free
    ]

instance RunMessage FollowingLeads where
  runMessage msg a@(FollowingLeads attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      players <- allPlayers
      lead <- getLeadPlayer
      push
        $ storyWithChooseOne
          lead
          players
          theTruth1
          [ Label "Tell Sergeant Monroe the truth." [DoStep 2 msg]
          , Label "Lie about your involvement." [DoStep 3 msg]
          ]
      pure a
    DoStep n msg'@(UseThisAbility _ (isSource attrs -> True) 1) -> do
      players <- allPlayers
      lead <- getLeadPlayer
      leadId <- getLead
      iids <- getInvestigators
      didn'tCoverAnythingUp <- noneM remembered [CleanedUpTheBlood, HidTheBody, TidiedUpTheRoom]
      coveredUpYourInvolvement <- allM remembered [CleanedUpTheBlood, HidTheBody, TidiedUpTheRoom]
      evidence <- selectCount $ AssetWithTrait Lead <> AssetWithClues (atLeast 2)
      noInnocents <- selectNone $ VictoryDisplayCardMatch $ CardWithTrait Innocent
      mSergeantMonroe <- selectOne $ assetIs Assets.sergeantMonroe
      let believableTruth = didn'tCoverAnythingUp && evidence == 2 && noInnocents
      let believableLie = coveredUpYourInvolvement && noInnocents

      case n of
        2 -> do
          pushAll [story players theTruth2, DoStep (if believableTruth then 5 else 4) msg']
        3 -> do
          pushAll [story players theTruth3, DoStep (if believableLie then 6 else 4) msg']
        4 -> do
          pushAll
            $ [ story players theTruth4
              , Remember ThePoliceDon'tBelieveYou
              ]
            <> [RemoveFromGame (toTarget sergeantMonroe) | sergeantMonroe <- maybeToList mSergeantMonroe]
            <> [ RemoveAllCopiesOfCardFromGame leadId (toCardCode Enemies.arkhamOfficer)
               , ShuffleEncounterDiscardBackIn
               , DoStep 7 msg'
               ]
        5 -> do
          takeControlMessage <- case mSergeantMonroe of
            Just sergeantMonroe -> pure $ chooseOrRunOne lead $ targetLabels iids $ only . (`TakeControlOfAsset` sergeantMonroe)
            Nothing -> do
              sergeantMonroe <- getSetAsideCard Assets.sergeantMonroe
              pure
                $ chooseOrRunOne lead
                $ targetLabels iids
                $ only
                . (`TakeControlOfSetAsideAsset` sergeantMonroe)
          pushAll
            [ story players theTruth5
            , Remember ThePoliceAreOnYourSide
            , takeControlMessage
            , RemoveAllCopiesOfCardFromGame leadId (toCardCode Enemies.arkhamOfficer)
            , ShuffleEncounterDiscardBackIn
            , DoStep 7 msg'
            ]
        6 -> do
          takeControlMessage <- case mSergeantMonroe of
            Just sergeantMonroe -> pure $ chooseOrRunOne lead $ targetLabels iids $ only . (`TakeControlOfAsset` sergeantMonroe)
            Nothing -> do
              sergeantMonroe <- getSetAsideCard Assets.sergeantMonroe
              pure
                $ chooseOrRunOne lead
                $ targetLabels iids
                $ only
                . (`TakeControlOfSetAsideAsset` sergeantMonroe)
          pushAll
            [ story players theTruth6
            , Remember ThePoliceAreOnYourSide
            , takeControlMessage
            , RemoveAllCopiesOfCardFromGame leadId (toCardCode Enemies.arkhamOfficer)
            , ShuffleEncounterDiscardBackIn
            , DoStep 7 msg'
            ]
        7 -> do
          mAlienDevice <- selectOne $ assetIs Assets.alienDevice
          mManagersKey <- selectOne $ assetIs Assets.managersKey
          mTomeOfRituals <- selectOne $ assetIs Assets.tomeOfRituals
          mSinisterSolution <- selectOne $ assetIs Assets.sinisterSolution
          mTimeWornLocket <- selectOne $ assetIs Assets.timeWornLocket
          case (mAlienDevice, mTimeWornLocket, mSinisterSolution, mManagersKey, mTomeOfRituals) of
            (Just alienDevice, Just timeWornLocket, _, _, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV1
              clues <- field AssetClues timeWornLocket
              room245 <- getJustLocationByName "Room 245"
              card <- genCard Enemies.vengefulSpecter
              placeVengefulSpecter <- createEnemyAt_ card room245 Nothing
              pushAll
                [ SetCurrentAgendaDeck 1 [agenda]
                , RemoveClues (toSource attrs) (toTarget timeWornLocket) clues
                , PlaceClues (toSource attrs) (toTarget alienDevice) clues
                , placeVengefulSpecter
                ]
            (Just alienDevice, _, Just sinisterSolution, _, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV2
              clues <- field AssetClues alienDevice
              hotelRoof <- getJustLocationByName "Hotel Roof"
              card <- genCard Enemies.otherworldlyMeddler
              (otherworldlyMeddler, placeOtherworldlyMeddler) <- createEnemyAt card hotelRoof Nothing
              doom <- perPlayer 1
              pushAll
                [ SetCurrentAgendaDeck 1 [agenda]
                , RemoveClues (toSource attrs) (toTarget alienDevice) clues
                , PlaceClues (toSource attrs) (toTarget sinisterSolution) clues
                , placeOtherworldlyMeddler
                , PlaceDoom (toSource attrs) (toTarget otherworldlyMeddler) doom
                ]
            (Just alienDevice, _, _, Just managersKey, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV3
              clues <- field AssetClues managersKey
              restaurant <- getJustLocationByName "Restaurant"
              card <- genCard Enemies.hotelManager
              placeHotelManager <- createEnemyAt_ card restaurant Nothing
              pushAll
                [ SetCurrentAgendaDeck 1 [agenda]
                , RemoveClues (toSource attrs) (toTarget managersKey) clues
                , PlaceClues (toSource attrs) (toTarget alienDevice) clues
                , placeHotelManager
                ]
            (Just alienDevice, _, _, _, Just tomeOfRituals) -> do
              agenda <- genCard Agendas.theTrueCulpritV4
              clues <- field AssetClues alienDevice
              hotelRoof <- getJustLocationByName "Hotel Roof"
              card <- genCard Enemies.otherworldlyMeddler
              (otherworldlyMeddler, placeOtherworldlyMeddler) <- createEnemyAt card hotelRoof Nothing
              doom <- perPlayer 2
              pushAll
                [ SetCurrentAgendaDeck 1 [agenda]
                , RemoveClues (toSource attrs) (toTarget alienDevice) clues
                , PlaceClues (toSource attrs) (toTarget tomeOfRituals) clues
                , placeOtherworldlyMeddler
                , PlaceDoom (toSource attrs) (toTarget otherworldlyMeddler) (2 + doom)
                ]
            (_, Just timeWornLocket, Just sinisterSolution, _, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV5
              clues <- field AssetClues timeWornLocket
              room245 <- getJustLocationByName "Room 245"
              card <- genCard Enemies.vengefulSpecter
              placeVengefulSpecter <- createEnemyAt_ card room245 Nothing
              pushAll
                [ SetCurrentAgendaDeck 1 [agenda]
                , RemoveClues (toSource attrs) (toTarget timeWornLocket) clues
                , PlaceClues (toSource attrs) (toTarget sinisterSolution) clues
                , placeVengefulSpecter
                ]
            (_, Just timeWornLocket, _, Just managersKey, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV6
              clues <- field AssetClues managersKey
              restaurant <- getJustLocationByName "Restaurant"
              card <- genCard Enemies.hotelManager
              placeHotelManager <- createEnemyAt_ card restaurant Nothing
              pushAll
                [ SetCurrentAgendaDeck 1 [agenda]
                , RemoveClues (toSource attrs) (toTarget managersKey) clues
                , PlaceClues (toSource attrs) (toTarget timeWornLocket) clues
                , placeHotelManager
                ]
            (_, Just _timeWornLocket, _, _, Just _tomeOfRituals) -> do
              agenda <- genCard Agendas.theTrueCulpritV7
              basement <- getJustLocationByName "Basement"
              card <- genCard Enemies.dimensionalShambler
              placeDimensionalShambler <- createEnemyAt_ card basement Nothing
              guests <- selectList $ VictoryDisplayCardMatch $ CardWithTrait Guest
              x <- perPlayer 1
              pushAll
                [ SetCurrentAgendaDeck 1 [agenda]
                , placeDimensionalShambler
                , ShuffleCardsIntoDeck Deck.EncounterDeck guests
                , ShuffleEncounterDiscardBackIn
                , DiscardUntilN (1 + x) leadId (toSource attrs) (toTarget attrs) Deck.EncounterDeck
                    $ BasicCardMatch
                    $ oneOf [CardWithTrait Guest, CardWithTrait Cultist]
                ]
            (_, _, Just _sinisterSolution, Just _managersKey, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV8
              staff <- selectList $ VictoryDisplayCardMatch $ CardWithTrait Staff
              room212 <- getJustLocationByName "Room 212"
              harvestedBrain <- getSetAsideCard Treacheries.harvestedBrain
              x <- getPlayerCount
              pushAll
                $ [ SetCurrentAgendaDeck 1 [agenda]
                  , ShuffleCardsIntoDeck Deck.EncounterDeck staff
                  , ShuffleEncounterDiscardBackIn
                  , DiscardUntilFirst leadId (toSource attrs) Deck.EncounterDeck
                      $ BasicCardMatch
                      $ CardWithTrait Staff
                  ]
                <> [ DiscardUntilFirst leadId (toSource attrs) Deck.EncounterDeck
                    $ BasicCardMatch
                    $ CardWithTrait Staff
                   | x >= 3
                   ]
                <> [AttachStoryTreacheryTo harvestedBrain (toTarget room212)]
            (_, _, Just sinisterSolution, _, Just tomeOfRituals) -> do
              agenda <- genCard Agendas.theTrueCulpritV9
              room212 <- getJustLocationByName "Room 212"
              clues <- field AssetClues sinisterSolution
              harvestedBrain <- getSetAsideCard Treacheries.harvestedBrain
              pushAll
                [ SetCurrentAgendaDeck 1 [agenda]
                , RemoveClues (toSource attrs) (toTarget sinisterSolution) clues
                , PlaceClues (toSource attrs) (toTarget tomeOfRituals) clues
                , AttachStoryTreacheryTo harvestedBrain (toTarget room212)
                ]
            (_, _, _, Just _managersKey, Just _tomeOfRituals) -> do
              agenda <- genCard Agendas.theTrueCulpritV10
              basement <- getJustLocationByName "Basement"
              card <- genCard Enemies.dimensionalShambler
              placeDimensionalShambler <- createEnemyAt_ card basement Nothing
              guests <- selectList $ VictoryDisplayCardMatch $ CardWithTrait Guest
              x <- perPlayer 2
              pushAll
                [ SetCurrentAgendaDeck 1 [agenda]
                , placeDimensionalShambler
                , ShuffleCardsIntoDeck Deck.EncounterDeck guests
                , ShuffleEncounterDiscardBackIn
                , PlaceDoom (toSource attrs) (AgendaMatcherTarget AnyAgenda) x
                ]
            _ -> error "invalid combination"

          pushAll
            [ story players theTruth7
            , RemoveAllDoomFromPlay defaultRemoveDoomMatchers
            , Discard GameSource (toTarget attrs)
            ]
        _ -> error "unknown step"

      pure a
    RequestedEncounterCards (isTarget attrs -> True) cards -> do
      crimeScenes <- selectList $ LocationWithTrait CrimeScene
      (emptyCrimeScenes, rest) <- partitionM (<=~> EmptyLocation) crimeScenes
      emptyCrimeScenes' <- shuffleM emptyCrimeScenes
      rest' <- shuffleM rest

      for_ (zip cards (emptyCrimeScenes' <> rest')) $ \(card, lid) -> do
        assetId <- getRandom
        push $ CreateAssetAt assetId (toCard card) (AtLocation lid)

      pure a
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      foyer <- getJustLocationByName "Foyer"
      pushM $ createEnemyAt_ (toCard ec) foyer Nothing
      pure a
    _ -> FollowingLeads <$> runMessage msg attrs
