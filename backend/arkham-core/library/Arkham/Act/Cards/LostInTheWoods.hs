module Arkham.Act.Cards.LostInTheWoods
  ( LostInTheWoods(..)
  , lostInTheWoods
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher hiding ( EncounterDeck )
import Arkham.Message
import Arkham.Movement
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Treacheries

newtype LostInTheWoods = LostInTheWoods ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LostInTheWoods where
  getModifiersFor (LocationTarget lid) (LostInTheWoods a) = do
    mInFrontOf <- field LocationInFrontOf lid
    pure $ toModifiers
      a
      [ ConnectedToWhen (LocationWithId lid)
        $ NotLocation (LocationWithId lid)
        <> LocationIsInFrontOf (InvestigatorWithId iid)
      | iid <- maybeToList mInFrontOf
      ]
  getModifiersFor (InvestigatorTarget iid) (LostInTheWoods a) = do
    lids <- selectList
      $ LocationIsInFrontOf (NotInvestigator $ InvestigatorWithId iid)
    pure $ toModifiers a $ map CannotEnter lids
  getModifiersFor _ _ = pure []

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
      iids <- getInvestigatorIds
      lead <- getLeadInvestigatorId
      arkhamWoods <- shuffleM
        =<< selectList (SetAsideCardMatch $ CardWithTitle "Arkham Woods")
      placements <- traverse placeLocation arkhamWoods

      goatSpawn <- getSetAsideCardsMatching $ cardIs Enemies.goatSpawn
      relentlessDarkYoung <- getSetAsideCard Enemies.relentlessDarkYoung

      let
        enemyPairings = if length iids == 4
          then
            (lead, relentlessDarkYoung) : zip (deleteFirst lead iids) goatSpawn
          else zip iids goatSpawn
        msgs =
          flip concatMap (zip iids placements) $ \(iid, (lid, placement)) ->
            [ placement
            , PutLocationInFrontOf iid lid
            , MoveTo $ uncancellableMove $ move attrs iid lid
            ]

      enemyMsgs <- flip concatMapM enemyPairings $ \(iid, enemyCard) -> do
        mlid <- field InvestigatorLocation iid
        case mlid of
          Nothing -> pure []
          Just lid -> do
            (enemyId, enemyCreation) <- createEnemyAt enemyCard lid Nothing
            pure
              [ enemyCreation
              , chooseOne
                iid
                [ SkillLabel
                    skillType
                    [beginSkillTest iid attrs enemyId skillType 3]
                | skillType <- [SkillWillpower, SkillAgility]
                ]
              ]

      piping <- mapMaybe (preview _EncounterCard)
        <$> getSetAsideCardsMatching (cardIs Treacheries.daemonicPiping)

      let
        pipingMsgs = case nonEmpty piping of
          Nothing -> error "no daemonic piping"
          Just (x :| xs) ->
            ShuffleCardsIntoDeck EncounterDeck [EncounterCard x]
              : map AddToEncounterDiscard xs

      pushAll $ msgs <> enemyMsgs <> pipingMsgs
      pure a
    PassedSkillTest iid _ (isSource attrs -> True) (EnemyTarget eid) _ _ -> do
      pushAll $ Exhaust (EnemyTarget eid) : [DisengageEnemy iid eid]
      pure a
    _ -> LostInTheWoods <$> runMessage msg attrs
