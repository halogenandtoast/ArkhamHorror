module Arkham.Act.Cards.ChildrenOfBlood.NewHorizons.TheSearchForAnswers (theSearchForAnswers) where

import Arkham.Ability
import Arkham.Act.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Locations
import Arkham.Matcher

newtype TheSearchForAnswers = TheSearchForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSearchForAnswers :: ActCard TheSearchForAnswers
theSearchForAnswers = act (2, A) TheSearchForAnswers Cards.theSearchForAnswers Nothing

instance HasAbilities TheSearchForAnswers where
  getAbilities = actAbilities \x ->
    [ onlyOnce $ restricted x 1 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    , restricted x 2 (CluesOnThis $ EqualTo $ Static 2) $ Objective $ forced $ RoundEnds #when
    ]

instance RunMessage TheSearchForAnswers where
  runMessage msg a@(TheSearchForAnswers attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      placeSetAsideLocation_ Locations.descendingTunnel
      advanceActDeck attrs
      pure a
    _ -> TheSearchForAnswers <$> liftRunMessage msg attrs
