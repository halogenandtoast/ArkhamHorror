module Arkham.Act.Cards.CurtainCall
  ( CurtainCall(..)
  , curtainCall
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Types
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype CurtainCall = CurtainCall ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curtainCall :: ActCard CurtainCall
curtainCall = act (3, A) CurtainCall Cards.curtainCall Nothing

instance HasAbilities CurtainCall where
  getAbilities (CurtainCall attrs) =
    [ restrictedAbility
        (ProxySource
          (LocationMatcherSource $ locationIs Cards.lobby)
          (toSource attrs)
        )
        1
        (Here
        <> Negate
             (EnemyCriteria $ EnemyExists $ enemyIs
               Enemies.theManInThePallidMask
             )
        )
      $ ActionAbility (Just Action.Resign)
      $ ActionCost 1
    , restrictedAbility
        attrs
        2
        (LocationExists
        $ LocationWithoutHorror
        <> AccessibleTo LocationWithAnyHorror
        )
      $ ForcedAbility
      $ RoundEnds Timing.When
    , restrictedAbility attrs 3 AllUndefeatedInvestigatorsResigned
      $ Objective
      $ ForcedAbility AnyWindow
    ]


instance RunMessage CurtainCall where
  runMessage msg a@(CurtainCall attrs) = case msg of
    UseCardAbility iid (ProxySource _ source) _ 1 _ | isSource attrs source ->
      a <$ push (Resign iid)
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      targets <-
        selectListMap LocationTarget
        $ LocationWithoutHorror
        <> AccessibleTo LocationWithAnyHorror
      a <$ pushAll (map (`PlaceHorror` 1) targets)
    UseCardAbility _ source _ 3 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      a <$ push
        (chooseOne
          leadInvestigatorId
          [ Label
            "We have to warn the police about what's going on! (-> R1)"
            [ScenarioResolution $ Resolution 1]
          , Label
            "The police won't believe us. We have to solve this mystery on our own. (-> R2)"
            [ScenarioResolution $ Resolution 2]
          ]
        )
    _ -> CurtainCall <$> runMessage msg attrs
