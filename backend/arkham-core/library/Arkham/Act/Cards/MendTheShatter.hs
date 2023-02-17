module Arkham.Act.Cards.MendTheShatter
  ( MendTheShatter(..)
  , mendTheShatter
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher hiding ( PlaceUnderneath, LocationCard )
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.SkillType
import Arkham.Target
import Arkham.Trait ( Trait (Shattered) )

newtype MendTheShatter = MendTheShatter ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mendTheShatter :: ActCard MendTheShatter
mendTheShatter = act (4, A) MendTheShatter Cards.mendTheShatter Nothing

instance HasAbilities MendTheShatter where
  getAbilities (MendTheShatter a) | onSide A a =
    [ restrictedAbility
        a
        1
        (InvestigatorExists $ You <> InvestigatorAt
          (LocationWithoutClues <> LocationWithTrait Shattered)
        )
      $ ActionAbility Nothing
      $ ActionCost 1
    , restrictedAbility
        a
        2
        (AssetExists $ AssetWithTitle "Relic of Ages" <> AssetWithCardsUnderneath
          (LengthIs $ AtLeast $ StaticWithPerPlayer 1 1)
        )
      $ Objective
      $ ForcedAbility AnyWindow
    ]
  getAbilities _ = []

instance RunMessage MendTheShatter where
  runMessage msg a@(MendTheShatter attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ chooseOne
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
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        lid <- getJustLocation iid
        card <- field LocationCard lid
        iids <- selectList $ colocatedWith iid
        enemyIds <- selectList $ UnengagedEnemy <> enemyAt lid
        aPocketInTime <- selectJust $ locationIs Locations.aPocketInTime
        relic <- selectJust $ AssetWithTitle "Relic of Ages"
        pushAll
          $ [ MoveTo (toSource attrs) iid' aPocketInTime | iid' <- iids ]
          <> [ EnemyMove eid lid | eid <- enemyIds ]
          <> [RemoveLocation lid, PlaceUnderneath (AssetTarget relic) [card]]
        pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      isRespossessThePast <- selectAny
        $ assetIs Assets.relicOfAgesRepossessThePast
      push $ ScenarioResolution $ Resolution $ if isRespossessThePast
        then 5
        else 1
      pure a
    _ -> MendTheShatter <$> runMessage msg attrs
