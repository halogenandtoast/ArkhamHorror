module Arkham.Event.Events.TaskForce (taskForce) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.ForMovement
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype Meta = Meta {usedOptions :: [Int]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TaskForce = TaskForce (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

taskForce :: EventCard TaskForce
taskForce = event (TaskForce . (`with` Meta [])) Cards.taskForce

instance RunMessage TaskForce where
  runMessage msg e@(TaskForce (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      doStep 0 msg
      pure e
    DoStep 0 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      canUseAbility <-
        (1 `notElem` usedOptions meta &&)
          <$> selectAny
            (AssetWithPerformableAbilityBy (affectsOthers $ colocatedWith iid) #action [IgnoreActionCost])
      canMove <-
        (2 `notElem` usedOptions meta &&)
          <$> selectAny
            ( affectsOthers
                $ colocatedWith iid
                <> InvestigatorCanMoveTo (toSource attrs) (ConnectedFrom ForMovement $ locationWithInvestigator iid)
            )
      canDiscover <-
        (3 `notElem` usedOptions meta &&)
          <$> selectAny
            ( locationWithInvestigator iid
                <> LocationWithDiscoverableCluesBy (affectsOthers $ colocatedWith iid)
            )

      when (canUseAbility || canMove || canDiscover) do
        chooseOneM iid do
          when canUseAbility do
            labeled "...resolve an {action} ability on an asset they control without paying its {action} cost."
              $ doStep 1 msg'
          when canMove $ labeled "...move to a connecting location." $ doStep 2 msg'
          when canDiscover $ labeled "...discover 1 clue at their location" $ doStep 3 msg'
      pure e
    DoStep 1 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      investigators <- select $ colocatedWith iid
      investigatorsWithAbilities <- flip mapMaybeM investigators \iid' -> do
        abilities <-
          map ((`applyAbilityModifiers` [IgnoreActionCost]) . doesNotProvokeAttacksOfOpportunity)
            <$> select
              ( PerformableAbilityBy (InvestigatorWithId iid') [IgnoreActionCost]
                  <> #action
                  <> AbilityOnAsset (assetControlledBy iid')
              )
        pure $ if null abilities then Nothing else Just (iid', abilities)
      chooseOrRunOneM iid do
        for_ investigatorsWithAbilities \(iid', abilities) ->
          targeting iid' $ chooseOrRunOneM iid' $ for_ abilities \ab -> abilityLabeled iid' ab nothing
      doStep 0 msg'
      pure . TaskForce $ attrs `with` Meta (1 : usedOptions meta)
    DoStep 2 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      investigators <- select $ colocatedWith iid
      investigatorsWithLocations <- forMaybeM investigators \iid' -> do
        locations <-
          select
            $ CanMoveToLocation
              (InvestigatorWithId iid')
              (toSource attrs)
              (ConnectedFrom ForMovement $ locationWithInvestigator iid')
        pure $ guard (notNull locations) $> (iid', locations)
      chooseOrRunOneM iid do
        for_ investigatorsWithLocations \(iid', locations) ->
          targeting iid' $ chooseOrRunOneM iid' $ targets locations $ moveTo attrs iid'
      doStep 0 msg'
      pure . TaskForce $ attrs `with` Meta (2 : usedOptions meta)
    DoStep 3 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      investigators <-
        filterM (\iid' -> iid' <=~> InvestigatorAt (locationWithDiscoverableCluesBy iid'))
          =<< select (colocatedWith iid)
      chooseOrRunOneM iid $ targets investigators \iid' -> discoverAtYourLocation NotInvestigate iid' attrs 1
      doStep 0 msg'
      pure . TaskForce $ attrs `with` Meta (3 : usedOptions meta)
    _ -> TaskForce . (`with` meta) <$> liftRunMessage msg attrs
