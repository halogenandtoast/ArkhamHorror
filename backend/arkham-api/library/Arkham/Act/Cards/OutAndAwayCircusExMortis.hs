module Arkham.Act.Cards.OutAndAwayCircusExMortis (outAndAwayCircusExMortis) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.CircusExMortis.Helpers (getSealedMoonTokens, releaseMoonToken)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype OutAndAwayCircusExMortis = OutAndAwayCircusExMortis ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outAndAwayCircusExMortis :: ActCard OutAndAwayCircusExMortis
outAndAwayCircusExMortis =
  act (3, A) OutAndAwayCircusExMortis Cards.outAndAwayCircusExMortis Nothing

instance HasAbilities OutAndAwayCircusExMortis where
  getAbilities (OutAndAwayCircusExMortis x) =
    [ mkAbility x 1 $ actionAbilityWithCost (HandDiscardCost 2 #any)
    , restricted x 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage OutAndAwayCircusExMortis where
  runMessage msg a@(OutAndAwayCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moons <- getSealedMoonTokens iid
      chooseOneM iid do
        for_ moons \token ->
          targeting (ChaosTokenTarget token) $ releaseMoonToken token
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> OutAndAwayCircusExMortis <$> liftRunMessage msg attrs
