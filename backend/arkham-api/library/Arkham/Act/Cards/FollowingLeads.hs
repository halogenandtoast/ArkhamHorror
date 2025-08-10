module Arkham.Act.Cards.FollowingLeads (followingLeads) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue
import Arkham.Helpers.Query (getInvestigators, getJustLocationByName)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
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
    [ restricted x 1 (AssetCount 2 $ withTrait Lead <> AssetWithClues (atLeast 1))
        $ Objective
        $ freeReaction (RoundEnds #when)
    ]

instance RunMessage FollowingLeads where
  runMessage msg a@(FollowingLeads attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advanceVia #other attrs (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      storyWithChooseOneM theTruth1 do
        labeled "Tell Sergeant Monroe the truth." $ doStep 2 msg
        labeled "Lie about your involvement." $ doStep 3 msg
      pure a
    DoStep n msg'@(AdvanceAct (isSide B attrs -> True) _ _) -> do
      lead <- getLead
      noInnocents <- selectNone $ VictoryDisplayCardMatch $ basic $ CardWithTrait Innocent
      mSergeantMonroe <- selectOne $ assetIs Assets.sergeantMonroe

      case n of
        2 -> do
          story theTruth2
          didn'tCoverAnythingUp <- noneM remembered [CleanedUpTheBlood, HidTheBody, TidiedUpTheRoom]
          evidence <- selectCount $ withTrait Lead <> AssetWithClues (atLeast 2)
          let believableTruth = didn'tCoverAnythingUp && evidence == 2 && noInnocents
          doStep (if believableTruth then 5 else 4) msg'
        3 -> do
          story theTruth3
          coveredUpYourInvolvement <- allM remembered [CleanedUpTheBlood, HidTheBody, TidiedUpTheRoom]
          let believableLie = coveredUpYourInvolvement && noInnocents
          doStep (if believableLie then 6 else 4) msg'
        4 -> do
          story theTruth4
          remember ThePoliceDon'tBelieveYou
          for_ mSergeantMonroe removeFromGame
          push $ RemoveAllCopiesOfCardFromGame lead (toCardCode Enemies.arkhamOfficer)
          shuffleEncounterDiscardBackIn
          doStep 7 msg'
        5 -> do
          story theTruth5
          remember ThePoliceAreOnYourSide

          iids <- getInvestigators
          case mSergeantMonroe of
            Just sergeantMonroe -> chooseOrRunOneM lead $ targets iids (`takeControlOfAsset` sergeantMonroe)
            Nothing -> do
              sergeantMonroe <- fetchCard Assets.sergeantMonroe
              chooseOrRunOneM lead $ targets iids (`takeControlOfSetAsideAsset` sergeantMonroe)
          push $ RemoveAllCopiesOfCardFromGame lead (toCardCode Enemies.arkhamOfficer)
          shuffleEncounterDiscardBackIn
          doStep 7 msg'
        6 -> do
          story theTruth6
          remember ThePoliceAreOnYourSide

          iids <- getInvestigators
          case mSergeantMonroe of
            Just sergeantMonroe -> do
              chooseOrRunOneM lead $ targets iids (`takeControlOfAsset` sergeantMonroe)
            Nothing -> do
              sergeantMonroe <- fetchCard Assets.sergeantMonroe
              chooseOrRunOneM lead $ targets iids (`takeControlOfSetAsideAsset` sergeantMonroe)
          push $ RemoveAllCopiesOfCardFromGame lead (toCardCode Enemies.arkhamOfficer)
          shuffleEncounterDiscardBackIn
          doStep 7 msg'
        7 -> do
          mAlienDevice <- selectOne $ assetIs Assets.alienDevice
          mManagersKey <- selectOne $ assetIs Assets.managersKey
          mTomeOfRituals <- selectOne $ assetIs Assets.tomeOfRituals
          mSinisterSolution <- selectOne $ assetIs Assets.sinisterSolution
          mTimeWornLocket <- selectOne $ assetIs Assets.timeWornLocket
          case (mAlienDevice, mTimeWornLocket, mSinisterSolution, mManagersKey, mTomeOfRituals) of
            (Just alienDevice, Just timeWornLocket, _, _, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV1
              push $ SetCurrentAgendaDeck 1 [agenda]
              clues <- field AssetClues timeWornLocket
              removeClues attrs timeWornLocket clues
              placeClues attrs alienDevice clues
              card <- fetchCard Enemies.vengefulSpecter
              createEnemyAtLocationMatching_ card "Room 245"
            (Just alienDevice, _, Just sinisterSolution, _, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV2
              push $ SetCurrentAgendaDeck 1 [agenda]
              clues <- field AssetClues alienDevice
              removeClues attrs alienDevice clues
              placeClues attrs sinisterSolution clues
              card <- fetchCard Enemies.otherworldlyMeddler
              otherworldlyMeddler <- createEnemyAtLocationMatching card "Hotel Roof"
              doom <- perPlayer 1
              placeDoom attrs otherworldlyMeddler doom
            (Just alienDevice, _, _, Just managersKey, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV3
              push $ SetCurrentAgendaDeck 1 [agenda]
              clues <- field AssetClues managersKey
              removeClues attrs managersKey clues
              placeClues attrs alienDevice clues
              card <- fetchCard Enemies.hotelManager
              createEnemyAtLocationMatching_ card "Restaurant"
            (Just alienDevice, _, _, _, Just tomeOfRituals) -> do
              agenda <- genCard Agendas.theTrueCulpritV4
              push $ SetCurrentAgendaDeck 1 [agenda]
              clues <- field AssetClues alienDevice
              removeClues attrs alienDevice clues
              placeClues attrs tomeOfRituals clues
              card <- fetchCard Enemies.otherworldlyMeddler
              otherworldlyMeddler <- createEnemyAtLocationMatching card "Hotel Roof"
              doom <- perPlayer 2
              placeDoom attrs otherworldlyMeddler (2 + doom)
            (_, Just timeWornLocket, Just sinisterSolution, _, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV5
              push $ SetCurrentAgendaDeck 1 [agenda]
              clues <- field AssetClues timeWornLocket
              removeClues attrs timeWornLocket clues
              placeClues attrs sinisterSolution clues
              card <- fetchCard Enemies.vengefulSpecter
              createEnemyAtLocationMatching_ card "Room 245"
            (_, Just timeWornLocket, _, Just managersKey, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV6
              push $ SetCurrentAgendaDeck 1 [agenda]
              clues <- field AssetClues managersKey
              removeClues attrs managersKey clues
              placeClues attrs timeWornLocket clues
              card <- fetchCard Enemies.hotelManager
              createEnemyAtLocationMatching_ card "Restaurant"
            (_, Just _timeWornLocket, _, _, Just _tomeOfRituals) -> do
              agenda <- genCard Agendas.theTrueCulpritV7
              push $ SetCurrentAgendaDeck 1 [agenda]
              card <- fetchCard Enemies.dimensionalShambler
              createEnemyAtLocationMatching_ card "Basement"
              guests <- select $ VictoryDisplayCardMatch $ basic $ CardWithTrait Guest
              shuffleCardsIntoDeck Deck.EncounterDeck guests
              shuffleEncounterDiscardBackIn
              x <- perPlayer 1
              discardUntilN (1 + x) lead attrs attrs Deck.EncounterDeck
                $ basic
                $ mapOneOf CardWithTrait [Guest, Cultist]
            (_, _, Just _sinisterSolution, Just _managersKey, _) -> do
              agenda <- genCard Agendas.theTrueCulpritV8
              push $ SetCurrentAgendaDeck 1 [agenda]
              staff <- select $ VictoryDisplayCardMatch $ basic $ withTrait Staff
              shuffleCardsIntoDeck Deck.EncounterDeck staff
              shuffleEncounterDiscardBackIn
              discardUntilFirst lead attrs Deck.EncounterDeck $ basic $ withTrait Staff
              x <- perPlayer 1
              when (x >= 3) do
                discardUntilFirst lead attrs Deck.EncounterDeck $ basic $ withTrait Staff
              tid <- getRandom
              room212 <- getJustLocationByName "Room 212"
              harvestedBrain <- fetchCard Treacheries.harvestedBrain
              push $ AttachStoryTreacheryTo tid harvestedBrain (toTarget room212)
            (_, _, Just sinisterSolution, _, Just tomeOfRituals) -> do
              agenda <- genCard Agendas.theTrueCulpritV9
              push $ SetCurrentAgendaDeck 1 [agenda]
              clues <- field AssetClues sinisterSolution
              removeClues attrs sinisterSolution clues
              placeClues attrs tomeOfRituals clues
              tid <- getRandom
              room212 <- getJustLocationByName "Room 212"
              harvestedBrain <- fetchCard Treacheries.harvestedBrain
              push $ AttachStoryTreacheryTo tid harvestedBrain (toTarget room212)
            (_, _, _, Just _managersKey, Just _tomeOfRituals) -> do
              agenda <- genCard Agendas.theTrueCulpritV10
              push $ SetCurrentAgendaDeck 1 [agenda]
              card <- fetchCard Enemies.dimensionalShambler
              createEnemyAtLocationMatching_ card "Basement"
              guests <- select $ VictoryDisplayCardMatch $ basic $ CardWithTrait Guest
              shuffleCardsIntoDeck Deck.EncounterDeck guests
              shuffleEncounterDiscardBackIn
              placeDoom attrs (AgendaMatcherTarget AnyAgenda) =<< perPlayer 2
            _ -> error "invalid combination"

          story theTruth7
          push $ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
          toDiscard GameSource attrs
        _ -> error "unknown step"

      pure a
    RequestedEncounterCards (isTarget attrs -> True) cards -> do
      crimeScenes <- select $ LocationWithTrait CrimeScene
      (emptyCrimeScenes, rest) <- partitionM (<=~> EmptyLocation) crimeScenes
      emptyCrimeScenes' <- shuffle emptyCrimeScenes
      rest' <- shuffle rest

      for_ (zip cards (emptyCrimeScenes' <> rest')) $ \(card, lid) -> do
        createEnemyAt_ (toCard card) lid

      pure a
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      createEnemyAtLocationMatching_ (toCard ec) "Foyer"
      pure a
    _ -> FollowingLeads <$> liftRunMessage msg attrs
