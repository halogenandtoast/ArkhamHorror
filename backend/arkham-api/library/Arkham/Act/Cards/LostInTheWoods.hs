module Arkham.Act.Cards.LostInTheWoods (
  LostInTheWoods (..),
  lostInTheWoods,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (EncounterDeck)
import Arkham.Movement
import Arkham.Projection
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Map.Strict qualified as Map

newtype LostInTheWoods = LostInTheWoods ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LostInTheWoods where
  getModifiersFor (LostInTheWoods a) = do
    locations <- modifySelectMaybe a (LocationIsInFrontOf Anyone) \lid -> do
      iid <- MaybeT $ field LocationInFrontOf lid
      pure
        [ ConnectedToWhen (LocationWithId lid)
            $ not_ (LocationWithId lid)
            <> LocationIsInFrontOf (InvestigatorWithId iid)
        ]

    investigators <- modifySelectMaybe a Anyone \iid -> do
      lids <- lift $ select $ LocationIsInFrontOf (not_ $ InvestigatorWithId iid)
      pure $ map CannotEnter lids

    pure $ locations <> investigators

instance HasAbilities LostInTheWoods where
  getAbilities (LostInTheWoods a) =
    [ mkAbility a 1
        $ Objective
        $ ReactionAbility (RoundEnds Timing.When)
        $ GroupClueCost (PerPlayer 2) Anywhere
    ]

lostInTheWoods :: ActCard LostInTheWoods
lostInTheWoods = act (1, A) LostInTheWoods Cards.lostInTheWoods Nothing

instance RunMessage LostInTheWoods where
  runMessage msg a@(LostInTheWoods attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithClues
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      iids <- getInvestigators
      lead <- getLead
      arkhamWoods <-
        shuffleM
          =<< select (SetAsideCardMatch $ CardWithTitle "Arkham Woods")
      placements <- traverse placeLocation arkhamWoods

      goatSpawn <- getSetAsideCardsMatching $ cardIs Enemies.goatSpawn
      relentlessDarkYoung <- getSetAsideCard Enemies.relentlessDarkYoung

      let
        placementMap = Map.fromList $ zip iids placements
        enemyPairings =
          if length iids == 4
            then (lead, relentlessDarkYoung) : zip (deleteFirst lead iids) goatSpawn
            else zip iids goatSpawn
        msgs =
          flip concatMap (zip iids placements) $ \(iid, (lid, placement)) ->
            [ placement
            , PutLocationInFrontOf iid lid
            , Move $ uncancellableMove $ move attrs iid lid
            ]

      enemyMsgs <- flip concatMapM enemyPairings $ \(iid, enemyCard) ->
        case lookup iid placementMap of
          Nothing -> pure []
          Just (lid, _) -> do
            (enemyId, enemyCreation) <- createEnemyAt enemyCard lid Nothing
            player <- getPlayer iid
            sid <- getRandom
            pure
              [ enemyCreation
              , chooseOne
                  player
                  [ SkillLabel skillType [beginSkillTest sid iid attrs enemyId skillType (Fixed 3)]
                  | skillType <- [SkillWillpower, SkillAgility]
                  ]
              ]

      piping <-
        mapMaybe (preview _EncounterCard)
          <$> getSetAsideCardsMatching (cardIs Treacheries.daemonicPiping)

      let
        pipingMsgs = case nonEmpty piping of
          Nothing -> error "no daemonic piping"
          Just (x :| xs) ->
            ShuffleCardsIntoDeck EncounterDeck [EncounterCard x]
              : map AddToEncounterDiscard xs

      pushAll
        $ msgs
        <> enemyMsgs
        <> pipingMsgs
        <> [AdvanceActDeck (actDeckId attrs) (toSource attrs)]
      pure a
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        mTarget <- getSkillTestTarget
        case mTarget of
          Just (EnemyTarget eid) ->
            pushAll $ Exhaust (EnemyTarget eid) : [DisengageEnemy iid eid]
          _ -> error "Invalid target"
        pure a
    _ -> LostInTheWoods <$> runMessage msg attrs
