module Arkham.Event.Events.Beguile (beguile) where

import Arkham.Ability
import Arkham.Constants
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype Beguile = Beguile EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beguile :: EventCard Beguile
beguile = event Beguile Cards.beguile

instance HasAbilities Beguile where
  getAbilities (Beguile x) = case x.placement.attachedTo of
    Just (EnemyTarget eid) ->
      [ controlled
          x
          1
          ( exists (be eid <> CanParleyEnemy You)
              <> oneOf
                [ exists (RevealedLocation <> LocationCanBeEnteredBy eid <> ConnectedFrom (locationWithEnemy eid))
                , exists
                    ( PerformableAbility [ActionCostModifier (-1), IgnoreOnSameLocation]
                        <> BasicAbility
                        <> oneOf
                          [ #investigate <> AbilityOnLocation (locationWithEnemy eid)
                          , #evade <> AbilityOnEnemy (at_ (locationWithEnemy eid))
                          ]
                    )
                ]
          )
          parleyAction_
      ]
    _ -> []

instance RunMessage Beguile where
  runMessage msg e@(Beguile attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ NonEliteEnemy <> enemyAtLocationWith iid
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (EnemyTarget eid) -> do
      push $ PlaceEvent attrs.id $ AttachedToEnemy eid
      pure e
    UseAbility iid ab _ | isSource attrs ab.source && ab.index == 1 -> do
      case attrs.placement.attachedTo of
        Just (EnemyTarget eid) -> do
          locations <-
            selectAny $ RevealedLocation <> LocationCanBeEnteredBy eid <> ConnectedFrom (locationWithEnemy eid)
          investigate' <-
            selectAny
              $ PerformableAbility [ActionCostModifier (-1), IgnoreOnSameLocation]
              <> BasicAbility
              <> #investigate
              <> AbilityOnLocation (locationWithEnemy eid)

          evade <-
            selectAny
              $ PerformableAbility [ActionCostModifier (-1), IgnoreOnSameLocation, IgnoreEngagementRequirement]
              <> BasicAbility
              <> #evade
              <> AbilityOnEnemy (at_ (locationWithEnemy eid))

          chooseOrRunOne iid
            $ [Label "Move attached enemy to a revealed connecting location" [DoStep 1 msg] | locations]
            <> [Label "Perform a basic investigate action at it's location" [DoStep 2 msg] | investigate']
            <> [Label "Perform a basic evade action at it's location" [DoStep 3 msg] | evade]
        _ -> error "Beguile: EnemyTarget not found"
      pure e
    DoStep n (UseAbility iid ab ws) | isSource attrs ab.source && ab.index == 1 -> do
      push . Do $ case n of
        1 -> UseAbility iid ab ws
        2 -> UseAbility iid (overAbilityActions (#investigate :) ab) ws
        3 -> UseAbility iid (overAbilityActions (#evade :) ab) ws
        _ -> error "Beguile: unexpected step"
      pure $ overAttrs (setMeta n) e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      case attrs.placement.attachedTo.enemy of
        Just eid -> case fromMaybe (1 :: Int) (getEventMeta attrs) of
          1 -> do
            locations <-
              select $ RevealedLocation <> LocationCanBeEnteredBy eid <> ConnectedFrom (locationWithEnemy eid)
            chooseOne iid [targetLabel location [EnemyMove eid location] | location <- locations]
          2 -> do
            field EnemyLocation eid >>= traverse_ \lid -> do
              abilities <-
                filter (and . sequence [abilityBasic, (== AbilityInvestigate) . abilityIndex])
                  <$> field LocationAbilities lid
              case abilities of
                [x] ->
                  pushAll
                    [ Msg.AbilityIsSkillTest $ AbilityRef (toSource attrs) 1
                    , UseAbility
                      iid
                      (overAbilityActions (const []) $ doesNotProvokeAttacksOfOpportunity $ decreaseAbilityActionCost x 1)
                      (defaultWindows iid)
                    ]
                _ -> error "expected exactly 1 investigate action on location"
          3 -> do
            field EnemyLocation eid >>= traverse_ \lid -> do
              sid <- getRandom
              push $ Msg.AbilityIsSkillTest $ AbilityRef (toSource attrs) 1
              chooseEvadeEnemyMatch sid iid (toSource iid) $ evadeOverride $ at_ (be lid)
          _ -> error "Beguile: unexpected step"
        _ -> error "Beguile: EnemyTarget not found"
      pure e
    FailedSkillTest {} -> do
      active <- selectAny $ ActiveAbility <> AbilityIs (toSource attrs) 1
      when active $ toDiscardBy attrs.owner attrs attrs
      pure e
    _ -> Beguile <$> liftRunMessage msg attrs
