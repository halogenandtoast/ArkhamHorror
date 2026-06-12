module Arkham.Act.Cards.LaidToRest (laidToRest) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator (getJustLocation)
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Scenarios.LaidToRest.Helpers

data Metadata = Metadata {advancedViaObjective :: Bool, flippedLocations :: [LocationId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LaidToRest = LaidToRest (ActAttrs `With` Metadata)
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laidToRest :: ActCard LaidToRest
laidToRest = act (1, A) (LaidToRest . (`with` Metadata False [])) Cards.laidToRest Nothing

instance HasAbilities LaidToRest where
  getAbilities (LaidToRest (a `With` meta)) =
    guard (onSide A a)
      *> [ scenarioI18n
             $ withI18nTooltip "laidToRest.moveEnemy"
             $ restricted a 1 (exists jimCulver <> exists (EnemyAt Anywhere))
             $ FastAbility
             $ OrCost
               [ ExhaustAssetCost (AssetWithTitle "Jim's Trumpet")
               , GroupClueCost (PerPlayer 1) Anywhere
               ]
         , scenarioI18n
             $ withI18nTooltip "laidToRest.flipLocation"
             $ restricted
               a
               2
               ( OnLocation
                   $ LocationWithoutModifier CannotBeFlipped
                   <> locationNotOneOf (flippedLocations meta)
               )
             $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
         , restricted a 3 (ExtendedCardCount (atLeast 4) $ VictoryDisplayCardMatch $ basic "Unfinished Business")
             $ Objective
             $ forced AnyWindow
         ]

instance RunMessage LaidToRest where
  runMessage msg act'@(LaidToRest (attrs `With` meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      jim <- selectJust jimCulver
      enemies <- select $ EnemyAt Anywhere
      chooseTargetM iid enemies \enemy -> moveToward enemy (locationWithInvestigator jim)
      pure act'
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      lid <- getJustLocation iid
      flipOverBy iid attrs lid
      pure . LaidToRest $ attrs `with` meta {flippedLocations = lid : flippedLocations meta}
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure . LaidToRest $ attrs `with` meta {advancedViaObjective = True}
    EndRound -> pure . LaidToRest $ attrs `with` meta {flippedLocations = []}
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      if advancedViaObjective meta
        then do
          lead <- getLead
          spirits <- select $ enemyIs Enemies.ravenousSpirit
          for_ spirits \spirit -> addToVictory lead spirit
          push R1
        else do
          n <- hereticsStillInTheBeyond
          mJim <- selectOne $ IncludeEliminated jimCulver
          for_ mJim \jim -> push $ Msg.SufferTrauma jim 0 (max 1 n)
          others <- select $ UneliminatedInvestigator <> not_ jimCulver
          for_ others \iid -> do
            push $ Msg.SufferTrauma iid 0 1
            push $ Msg.InvestigatorDefeated (toSource attrs) iid
          push R2
      pure act'
    _ -> LaidToRest . (`with` meta) <$> liftRunMessage msg attrs
