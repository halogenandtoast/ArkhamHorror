module Arkham.Act.Cards.TheWayOut (TheWayOut (..), theWayOut) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype TheWayOut = TheWayOut ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWayOut :: ActCard TheWayOut
theWayOut = act (3, A) TheWayOut Cards.theWayOut Nothing

instance HasAbilities TheWayOut where
  getAbilities (TheWayOut a)
    | onSide A a =
        [ restrictedAbility a 1 (exists $ locationIs Locations.theGateToHell) $ forced $ RoundEnds #when
        , restrictedAbility a 2 (EachUndefeatedInvestigator $ at_ $ locationIs Locations.theGateToHell)
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage TheWayOut where
  runMessage msg a@(TheWayOut attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      lead <- getLead
      theGateToHell <- selectJust $ locationIs $ Locations.theGateToHell
      selectOrRunOneToHandle lead (attrs.ability 1) $ FarthestLocationFromLocation theGateToHell Anywhere
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    HandleTargetChoice _iid (isSource attrs -> True) (LocationTarget lid) -> do
      investigators <- select $ investigatorAt lid
      enemies <- select $ enemyAt lid
      when (notNull investigators || notNull enemies) do
        lead <- getLead
        connectedLocations <- select $ accessibleFrom lid
        chooseOrRunOneM lead do
          targets connectedLocations \connected -> do
            chooseOneAtATimeM lead do
              targets investigators \investigator -> moveTo attrs investigator connected
              targets enemies \enemy -> push $ EnemyMove enemy connected
      toDiscard (attrs.ability 1) lid
      pure a
    _ -> TheWayOut <$> liftRunMessage msg attrs
