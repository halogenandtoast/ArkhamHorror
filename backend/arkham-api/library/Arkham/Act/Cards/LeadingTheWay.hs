module Arkham.Act.Cards.LeadingTheWay (
  LeadingTheWay (..),
  leadingTheWay,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Resolution

newtype LeadingTheWay = LeadingTheWay ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadingTheWay :: ActCard LeadingTheWay
leadingTheWay = act (3, A) LeadingTheWay Cards.leadingTheWay Nothing

instance HasModifiersFor LeadingTheWay where
  getModifiersFor (LocationTarget lid) (LeadingTheWay attrs) = do
    isBlockedPassage <-
      elem lid
        <$> select (locationIs Locations.blockedPassage)
    pure $ toModifiers attrs [Blank | isBlockedPassage]
  getModifiersFor _ _ = pure []

instance HasAbilities LeadingTheWay where
  getAbilities (LeadingTheWay a)
    | onSide A a =
        [ restrictedAbility
            a
            1
            ( EachUndefeatedInvestigator
                $ InvestigatorAt
                $ locationIs
                  Locations.blockedPassage
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage LeadingTheWay where
  runMessage msg a@(LeadingTheWay attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push (AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther)
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 2
      pure a
    _ -> LeadingTheWay <$> runMessage msg attrs
