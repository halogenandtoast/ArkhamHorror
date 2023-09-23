module Arkham.Act.Cards.Timelock (
  Timelock (..),
  timelock,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard, PlaceUnderneath)
import Arkham.Message
import Arkham.Movement
import Arkham.Projection
import Arkham.Resolution
import Arkham.SkillType
import Arkham.Trait (Trait (Shattered))

newtype Timelock = Timelock ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timelock :: ActCard Timelock
timelock = act (4, A) Timelock Cards.timelock Nothing

instance HasAbilities Timelock where
  getAbilities (Timelock a)
    | onSide A a =
        [ restrictedAbility
            a
            1
            ( InvestigatorExists
                $ You
                <> InvestigatorAt
                  (LocationWithoutClues <> LocationWithTrait Shattered)
            )
            $ ActionAbility Nothing
            $ ActionCost 1
        , restrictedAbility
            a
            2
            ( AssetExists
                $ AssetWithTitle "Relic of Ages"
                <> AssetWithCardsUnderneath
                  (HasCard $ cardIs Locations.pnakotus)
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage Timelock where
  runMessage msg a@(Timelock attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ chooseOne
          iid
          [ SkillLabel
            skillType
            [ beginSkillTest
                iid
                (toSource attrs)
                (toTarget attrs)
                skillType
                3
            ]
          | skillType <- [SkillWillpower, SkillIntellect]
          ]
      pure a
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        lid <- getJustLocation iid
        card <- field LocationCard lid
        iids <- selectList $ colocatedWith iid
        enemyIds <- selectList $ UnengagedEnemy <> enemyAt lid
        aPocketInTime <- selectJust $ locationIs Locations.aPocketInTime
        relic <- selectJust $ AssetWithTitle "Relic of Ages"
        pushAll
          $ [MoveTo $ move (toSource attrs) iid' aPocketInTime | iid' <- iids]
          <> [EnemyMove eid lid | eid <- enemyIds]
          <> [RemoveLocation lid, PlaceUnderneath (AssetTarget relic) [card]]
        pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 3
      pure a
    _ -> Timelock <$> runMessage msg attrs
