module Arkham.Act.Cards.LeadingTheWay (leadingTheWay) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype LeadingTheWay = LeadingTheWay ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

leadingTheWay :: ActCard LeadingTheWay
leadingTheWay = act (3, A) LeadingTheWay Cards.leadingTheWay Nothing

instance HasModifiersFor LeadingTheWay where
  getModifiersFor (LeadingTheWay attrs) = do
    modifySelect attrs (locationIs Locations.blockedPassage) [Blank]

instance HasAbilities LeadingTheWay where
  getAbilities (LeadingTheWay a) =
    [ restricted a 1 (EachUndefeatedInvestigator $ at_ $ locationIs Locations.blockedPassage)
      $ Objective
      $ forced AnyWindow
    | onSide A a
    ]

instance RunMessage LeadingTheWay where
  runMessage msg a@(LeadingTheWay attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> LeadingTheWay <$> liftRunMessage msg attrs
