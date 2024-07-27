module Arkham.Event.Cards.Beguile (beguile, Beguile (..)) where

import Arkham.Ability
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection

newtype Beguile = Beguile EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beguile :: EventCard Beguile
beguile = event Beguile Cards.beguile

instance HasAbilities Beguile where
  getAbilities (Beguile x) = case x.placement.attachedTo of
    Just (EnemyTarget eid) ->
      [ controlledAbility
          x
          1
          ( exists (EnemyWithId eid <> CanParleyEnemy You)
              <> oneOf
                [ exists (RevealedLocation <> LocationCanBeEnteredBy eid <> ConnectedFrom (locationWithEnemy eid))
                , exists
                    ( PerformableAbility [ActionCostModifier (-1)]
                        <> BasicAbility
                        <> oneOf [AbilityIsAction #investigate, AbilityIsAction #evade]
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
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      push $ PlaceEvent iid attrs.id $ AttachedToEnemy eid
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      case attrs.placement.attachedTo of
        Just (EnemyTarget eid) -> do
          locations <-
            selectAny $ RevealedLocation <> LocationCanBeEnteredBy eid <> ConnectedFrom (locationWithEnemy eid)
          investigate <- selectAny $ InvestigatableLocation <> locationWithEnemy eid
          evade <- selectAny $ EnemyCanBeEvadedBy (toSource iid)
          chooseOrRunOne
            iid
            $ [Label "Move attached enemy to a revealed connecting location" [DoStep 1 msg] | locations]
            <> [Label "Perform a basic investigate action at it's location" [DoStep 2 msg] | investigate]
            <> [Label "Perform a basic evade action at it's location" [DoStep 3 msg] | evade]
        _ -> error "Beguile: EnemyTarget not found"
      pure e
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      case attrs.placement.attachedTo of
        Just (EnemyTarget eid) -> do
          locations <-
            select $ RevealedLocation <> LocationCanBeEnteredBy eid <> ConnectedFrom (locationWithEnemy eid)
          chooseOne iid [targetLabel location [EnemyMove eid location] | location <- locations]
        _ -> error "Beguile: EnemyTarget not found"
      pure e
    DoStep 2 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      case attrs.placement.attachedTo of
        Just (EnemyTarget eid) ->
          field EnemyLocation eid >>= traverse_ \lid -> do
            sid <- getRandom
            pushM $ mkInvestigateLocation sid iid (toSource iid) lid
        _ -> error "Beguile: EnemyTarget not found"
      pure e
    DoStep 3 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      case attrs.placement.attachedTo of
        Just (EnemyTarget eid) ->
          field EnemyLocation eid >>= traverse_ \lid -> do
            sid <- getRandom
            chooseEvadeEnemyMatch sid iid (toSource iid) $ evadeOverride $ EnemyAt $ LocationWithId lid
        _ -> error "Beguile: EnemyTarget not found"
      pure e
    FailedSkillTest {} -> do
      active <- selectAny $ ActiveAbility <> AbilityIs (toSource attrs) 1
      when active $ toDiscardBy attrs.owner attrs attrs
      pure e
    _ -> Beguile <$> liftRunMessage msg attrs
