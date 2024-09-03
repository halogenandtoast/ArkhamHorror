module Arkham.Event.Cards.TaskForce (taskForce, TaskForce (..)) where

import Arkham.Ability
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (discoverAtYourLocation)
import Arkham.Helpers.Query (getPlayer)
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Message qualified as Msg
import Arkham.Modifier
import Arkham.Movement

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
            ( AssetWithPerformableAbilityBy
                (affectsOthers $ colocatedWith iid)
                AbilityIsActionAbility
                [IgnoreActionCost]
            )
      canMove <-
        (2 `notElem` usedOptions meta &&)
          <$> selectAny
            ( affectsOthers
                $ colocatedWith iid
                <> InvestigatorCanMoveTo (toSource attrs) (ConnectedFrom $ locationWithInvestigator iid)
            )
      canDiscover <-
        (3 `notElem` usedOptions meta &&)
          <$> selectAny
            ( locationWithInvestigator iid
                <> LocationWithDiscoverableCluesBy (affectsOthers $ colocatedWith iid)
            )

      when (canUseAbility || canMove || canDiscover) do
        chooseOne iid
          $ [ Label
              "...resolve an {action} ability on an asset they control without paying its {action} cost."
              [DoStep 1 msg']
            | canUseAbility
            ]
          <> [Label "...move to a connecting location." [DoStep 2 msg'] | canMove]
          <> [Label "...discover 1 clue at their location" [DoStep 3 msg'] | canDiscover]

      pure e
    DoStep 1 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      investigators <- select $ colocatedWith iid
      investigatorsWithAbilities <- flip mapMaybeM investigators \iid' -> do
        abilities <-
          map ((`applyAbilityModifiers` [IgnoreActionCost]) . doesNotProvokeAttacksOfOpportunity)
            <$> select
              ( PerformableAbilityBy (InvestigatorWithId iid') [IgnoreActionCost]
                  <> AbilityIsActionAbility
                  <> AbilityOnAsset (assetControlledBy iid')
              )
        player <- getPlayer iid'
        pure $ if null abilities then Nothing else Just (player, iid', abilities)
      chooseOrRunOne
        iid
        [ targetLabel iid' [Msg.chooseOrRunOne player [AbilityLabel iid' ab [] [] [] | ab <- abilities]]
        | (player, iid', abilities) <- investigatorsWithAbilities
        ]
      doStep 0 msg'
      pure . TaskForce $ attrs `with` Meta (1 : usedOptions meta)
    DoStep 2 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      investigators <- select $ colocatedWith iid
      investigatorsWithLocations <- flip mapMaybeM investigators \iid' -> do
        locations <-
          select
            $ CanMoveToLocation
              (InvestigatorWithId iid')
              (toSource attrs)
              (ConnectedFrom $ locationWithInvestigator iid')
        player <- getPlayer iid'
        pure $ if null locations then Nothing else Just (player, iid', locations)
      chooseOrRunOne
        iid
        [ targetLabel
          iid'
          [Msg.chooseOrRunOne player [targetLabel lid [Move $ move attrs iid' lid] | lid <- locations]]
        | (player, iid', locations) <- investigatorsWithLocations
        ]
      doStep 0 msg'
      pure . TaskForce $ attrs `with` Meta (2 : usedOptions meta)
    DoStep 3 msg'@(PlayThisEvent iid (is attrs -> True)) -> do
      investigators <-
        filterM (\iid' -> iid' <=~> InvestigatorAt (locationWithDiscoverableCluesBy iid'))
          =<< select (colocatedWith iid)
      chooseOrRunOne
        iid
        [ targetLabel iid' [DiscoverClues iid' $ discoverAtYourLocation attrs 1]
        | iid' <- investigators
        ]
      doStep 0 msg'
      pure . TaskForce $ attrs `with` Meta (3 : usedOptions meta)
    _ -> TaskForce . (`with` meta) <$> liftRunMessage msg attrs
