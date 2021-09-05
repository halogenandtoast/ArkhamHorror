module Arkham.Types.Act.Cards.CurtainCall
  ( CurtainCall(..)
  , curtainCall
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import qualified Arkham.Enemy.Cards as Cards
import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype CurtainCall = CurtainCall ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
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
        Here
      $ ActionAbility (Just Action.Resign)
      $ ActionCost 1
    , restrictedAbility
        attrs
        2
        (LocationExists
        $ LocationWithoutHorror
        <> AccessibleFrom LocationWithAnyHorror
        )
      $ ForcedAbility
      $ RoundEnds Timing.When
    , restrictedAbility attrs 3 AllUndefeatedInvestigatorsResigned
      $ Objective
      $ ForcedAbility AnyWindow
    ]


instance ActRunner env => RunMessage env CurtainCall where
  runMessage msg a@(CurtainCall attrs) = case msg of
    UseCardAbility iid (ProxySource _ source) _ 1 _ | isSource attrs source ->
      a <$ push (Resign iid)
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      targets <-
        selectListMap LocationTarget
        $ LocationWithoutHorror
        <> AccessibleFrom LocationWithAnyHorror
      a <$ pushAll (map (`PlaceHorror` 1) targets)
    UseCardAbility _ source _ 3 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
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
