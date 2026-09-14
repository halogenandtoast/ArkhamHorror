module Arkham.Homebrew.CircusExMortis.Treacheries.FocusedSabotage (focusedSabotage) where

import Arkham.Ability
import {-# SOURCE #-} Arkham.GameEnv (getPhase)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Phase
import Arkham.Treachery.Import.Lifted hiding (EnemyEvaded)
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

-- | (last phase this reacted in, enemies it already reacted to during that phase)
newtype Meta = Meta {triggered :: (Maybe Phase, [EnemyId])}
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)

newtype FocusedSabotage = FocusedSabotage TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

focusedSabotage :: TreacheryCard FocusedSabotage
focusedSabotage = treachery FocusedSabotage Cards.focusedSabotage

instance HasAbilities FocusedSabotage where
  getAbilities (FocusedSabotage attrs) = case attrs.placement of
    NextToAgenda ->
      [ mkAbility attrs 1 $ forced $ PlacedDoomCounter #after AnySource (EnemyTargetMatches AnyEnemy)
      , mkAbility attrs 2 $ forced $ EnemyEvaded #after Anyone EnemyWithAnyDoom
      ]
    _ -> []

toDoomedEnemy :: [Window] -> EnemyId
toDoomedEnemy [] = error "invalid state"
toDoomedEnemy ((windowType -> Window.PlacedDoom _ (EnemyTarget eid) _) : _) = eid
toDoomedEnemy (_ : xs) = toDoomedEnemy xs

instance RunMessage FocusedSabotage where
  runMessage msg t@(FocusedSabotage attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      place attrs NextToAgenda
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (toDoomedEnemy -> eid) _ -> do
      phase <- getPhase
      let Meta (lastPhase, seen) = toResultDefault (Meta (Nothing, [])) attrs.meta
      let seenThisPhase = if lastPhase == Just phase then seen else []
      if eid `elem` seenThisPhase
        then pure t
        else do
          nearest <- select $ NearestToEnemy (EnemyWithId eid)
          chooseOrRunOneM iid $ targets nearest $ initiateEnemyAttack eid attrs
          pure . FocusedSabotage $ setMeta (Meta (Just phase, eid : seenThisPhase)) attrs
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      toDiscardBy iid attrs attrs
      pure t
    _ -> FocusedSabotage <$> liftRunMessage msg attrs
