module Arkham.Act.Cards.DescentIntoDark (descentIntoDark) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype DescentIntoDark = DescentIntoDark ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descentIntoDark :: ActCard DescentIntoDark
descentIntoDark = act (2, A) DescentIntoDark Cards.descentIntoDark Nothing

instance HasAbilities DescentIntoDark where
  getAbilities (DescentIntoDark a) =
    [ restricted
        a
        1
        ( notExists (InvestigatorAt $ not_ $ locationIs Locations.descentToYoth)
            <> exists (locationIs Locations.descentToYoth <> LocationWithoutDoom)
        )
        $ Objective
        $ FastAbility Free
    ]

instance RunMessage DescentIntoDark where
  runMessage msg a@(DescentIntoDark attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
      push $ if rescuedAlejandro then R1 else R2
      pure a
    _ -> DescentIntoDark <$> liftRunMessage msg attrs
