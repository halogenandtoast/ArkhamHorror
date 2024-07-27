module Arkham.Event.Cards.FalseSurrender (falseSurrender, FalseSurrender (..)) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getCanPerformAbility)
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (defaultWindows)

newtype Meta = Meta {chosenEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype FalseSurrender = FalseSurrender (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseSurrender :: EventCard FalseSurrender
falseSurrender = event (FalseSurrender . (`with` Meta Nothing)) Cards.falseSurrender

instance HasModifiersFor FalseSurrender where
  getModifiersFor (InvestigatorTarget iid) (FalseSurrender (With attrs meta)) = maybeModified attrs do
    guard $ iid == attrs.owner
    eid <- hoistMaybe $ chosenEnemy meta
    pure [MustChooseEnemy (EnemyWithId eid)]
  getModifiersFor _ _ = pure []

instance RunMessage FalseSurrender where
  runMessage msg e@(FalseSurrender (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ enemyAtLocationWith iid <> canParleyEnemy iid
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      selectOneToHandle iid attrs
        $ PlayableCardWithCostReduction NoAction 1
        $ inHandOf iid
        <> basic (#asset <> #weapon)
      pure . FalseSurrender $ With attrs (Meta $ Just eid)
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      putCardIntoPlay iid =<< getCard cid
      doStep 2 msg
      pure e
    DoStep 2 (HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid)) -> do
      whenJustM (selectOne $ AssetWithCardId cid) \asset -> do
        let nullifyActionCost ab = applyAbilityModifiers ab [ActionCostSetToModifier 0]
        abilities <-
          filterM (getCanPerformAbility iid (defaultWindows iid))
            =<< selectMap
              nullifyActionCost
              ( AbilityIsAction #fight
                  <> AssetAbility (AssetWithId asset)
              )
        when (notNull abilities) do
          chooseOneM iid do
            labeled "Take a fight action against that enemy" do
              chooseOne iid [AbilityLabel iid ab [] [] [] | ab <- abilities]
            labeled "Do not take a fight action" $ doStep 3 msg
      pure e
    DoStep 3 (HandleTargetChoice _ (isSource attrs -> True) _) -> do
      pure . FalseSurrender $ With attrs (Meta Nothing)
    ChoseEnemy sid iid _ _ -> do
      when (isJust $ chosenEnemy meta) do
        skillTestModifiers
          sid
          (attrs.ability 1)
          iid
          [ UseSkillInsteadOf #combat #agility
          , UseSkillInsteadOf #intellect #agility
          , UseSkillInsteadOf #willpower #agility
          ]
      pure . FalseSurrender $ With attrs (Meta Nothing)
    _ -> FalseSurrender . (`with` meta) <$> liftRunMessage msg attrs
