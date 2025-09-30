module Arkham.Location.Cards.ReturnToSouthside (returnToSouthside) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card.Id
import Arkham.Enemy.Creation
import Arkham.Evade.Types
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype ReturnToSouthside = ReturnToSouthside LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToSouthside :: LocationCard ReturnToSouthside
returnToSouthside = location ReturnToSouthside Cards.returnToSouthside 3 (Static 0)

instance HasModifiersFor ReturnToSouthside where
  getModifiersFor (ReturnToSouthside a) = do
    eachInvestigator \iid ->
      modified_
        a
        (AbilityTarget iid $ AbilityRef (toSource a) 1)
        [ CanModify
            $ EnemyEvadeActionCriteria
            $ CriteriaOverride
            $ EnemyCriteria
            $ ThisEnemy
            $ InPlayEnemy
            $ EnemyWithoutModifier CannotBeEvaded
        , CanEvadeOverride $ CriteriaOverride $ exists $ InEncounterDiscard <> #enemy
        ]

instance HasAbilities ReturnToSouthside where
  getAbilities (ReturnToSouthside a) = extendRevealed1 a $ restricted a 1 Here evadeAction_

instance RunMessage ReturnToSouthside where
  runMessage msg l@(ReturnToSouthside attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      enemyCards <- select $ InEncounterDiscard <> #enemy
      let
        toEvadeLabel card =
          let eid = unsafeFromCardId card.id
           in EvadeLabel
                eid
                [ ChosenEvadeEnemy sid (attrs.ability 1) eid
                , EvadeEnemy sid iid eid (attrs.ability 1) (Just $ toTarget attrs) #agility False
                , RemoveEnemy eid
                ]
      let wrapper body = if null enemyCards then body else focusCards enemyCards (lift body)
      wrapper do
        chooseEvadeEnemyEdit sid iid (attrs.ability 1) \c ->
          c
            { chooseEvadeEnemyMatcher = evadeOverride $ InPlayEnemy $ EnemyWithoutModifier CannotBeEvaded
            , chooseEvadeOverride = True
            , chooseEvadeTarget = Just $ toTarget attrs
            , chooseEvadeAdditionalOptions = map toEvadeLabel enemyCards
            }
      pure l
    Successful (Action.Evade, EnemyTarget _) _iid _ (isTarget attrs -> True) _ -> do
      let n = countLocationBreaches attrs
      act <- selectJust AnyAct
      removeBreaches attrs n
      placeBreaches act n
      pure l
    Failed (Action.Evade, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      initiateEnemyAttack eid (attrs.ability 1) iid
      pure l
    ChosenEvadeEnemy _ (isAbilitySource attrs 1 -> True) eid -> do
      enemyCards <- select $ InEncounterDiscard <> #enemy
      for_ (find ((== eid) . unsafeFromCardId . (.id)) enemyCards) \card -> do
        msg' <- Msg.createEnemy card StillInEncounterDiscard
        Msg.push $ msg' {enemyCreationEnemyId = eid}
      pure l
    _ -> ReturnToSouthside <$> liftRunMessage msg attrs
