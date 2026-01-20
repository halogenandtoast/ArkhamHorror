module Arkham.Act.Cards.TheFinalMirage (theFinalMirage) where

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Key
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), hasModifier, modifySelect, modifySelf)
import Arkham.Keyword (Keyword (Hunter))
import Arkham.Matcher
import Arkham.Window (getBatchId)

newtype TheFinalMirage = TheFinalMirage ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFinalMirage :: ActCard TheFinalMirage
theFinalMirage = act (4, A) TheFinalMirage Cards.theFinalMirage Nothing

instance HasModifiersFor TheFinalMirage where
  getModifiersFor (TheFinalMirage a) = do
    modifySelf a [ActIsAgenda]
    modifySelect a (enemyIs Enemies.theNamelessMadness) [AddKeyword Hunter]

instance HasAbilities TheFinalMirage where
  getAbilities (TheFinalMirage a) =
    extend1 a
      $ restricted a 1 (SetAsideCardExists $ cardIs Enemies.theNamelessMadness)
      $ forced
      $ WouldPlaceDoomCounter #when #any #any

instance RunMessage TheFinalMirage where
  runMessage msg a@(TheFinalMirage attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      hasAPlan <- getHasRecord DrKenslerHasAPlan
      eluded <- getHasRecord TheTruthOfTheMirageEludesYou
      collapsed <- hasModifier attrs (ScenarioModifier "collapsed")
      push
        $ if
          | collapsed && hasAPlan -> R1
          | collapsed && eluded -> R2
          | otherwise -> R3
      pure a
    UseCardAbility _iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      push $ IgnoreBatch batchId
      mleftmost <-
        selectOne
          $ FirstLocation
          $ map
            ((<> LocationWithInvestigator Anyone) . LocationWithLabel)
            ["theGateOfYquaa", "titanicRamp1", "titanicRamp2", "titanicRamp3", "titanicRamp4", "hiddenTunnel"]
      for_ mleftmost (createSetAsideEnemy_ Enemies.theNamelessMadness)
      pure a
    PlaceTokens s (AgendaTarget aid) tkn n | toCardCode aid == toCardCode attrs -> do
      TheFinalMirage <$> liftRunMessage (PlaceTokens s (ActTarget $ toId attrs) tkn n) attrs
    _ -> TheFinalMirage <$> liftRunMessage msg attrs
