module Arkham.Act.Cards.LostInTheWoods (lostInTheWoods) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (EncounterDeck)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Movement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Map.Strict qualified as Map

newtype LostInTheWoods = LostInTheWoods ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor LostInTheWoods where
  getModifiersFor (LostInTheWoods a) = do
    modifySelectMaybe a (LocationIsInFrontOf Anyone) \lid -> do
      iid <- MaybeT $ field LocationInFrontOf lid
      pure [ConnectedToWhen (be lid) $ not_ (be lid) <> LocationIsInFrontOf (be iid)]

    modifySelectMaybe a Anyone \iid -> do
      lids <- lift $ select $ LocationIsInFrontOf (not_ $ be iid)
      pure $ map CannotEnter lids

instance HasAbilities LostInTheWoods where
  getAbilities (LostInTheWoods a) =
    [mkAbility a 1 $ Objective $ triggered (RoundEnds #when) $ GroupClueCost (PerPlayer 2) Anywhere]

lostInTheWoods :: ActCard LostInTheWoods
lostInTheWoods = act (1, A) LostInTheWoods Cards.lostInTheWoods Nothing

instance RunMessage LostInTheWoods where
  runMessage msg a@(LostInTheWoods attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      iids <- getInvestigators
      lead <- getLead

      goatSpawn <- getSetAsideCardsMatching $ cardIs Enemies.goatSpawn
      relentlessDarkYoung <- getSetAsideCard Enemies.relentlessDarkYoung

      arkhamWoods <-
        shuffleM
          =<< select (SetAsideCardMatch $ CardWithTitle "Arkham Woods")
      placements <- traverse placeLocation arkhamWoods

      placementMap <-
        Map.fromList <$> for (zip iids placements) \(iid, lid) -> do
          push $ PutLocationInFrontOf iid lid
          moveToEdit attrs iid lid uncancellableMove
          pure (iid, lid)

      let
        enemyPairings =
          if length iids == 4
            then (lead, relentlessDarkYoung) : zip (deleteFirst lead iids) goatSpawn
            else zip iids goatSpawn
      for_ enemyPairings \(iid, enemyCard) ->
        for_ (lookup iid placementMap) \lid -> do
          enemyId <- createEnemyAt enemyCard lid
          sid <- getRandom
          chooseBeginSkillTest sid iid attrs enemyId [#willpower, #agility] (Fixed 3)

      piping <-
        mapMaybe (preview _EncounterCard)
          <$> getSetAsideCardsMatching (cardIs Treacheries.daemonicPiping)

      case nonEmpty piping of
        Nothing -> error "no daemonic piping"
        Just (x :| xs) -> do
          shuffleCardsIntoDeck EncounterDeck (only x)
          addToEncounterDiscard xs

      advanceActDeck attrs
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTargetedEnemy >>= \case
        Just eid -> do
          exhaustThis eid
          disengageEnemy iid eid
        _ -> error "Invalid target"
      pure a
    _ -> LostInTheWoods <$> liftRunMessage msg attrs
