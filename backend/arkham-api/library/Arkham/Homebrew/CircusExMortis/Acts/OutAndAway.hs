module Arkham.Homebrew.CircusExMortis.Acts.OutAndAway (outAndAway) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens, releaseMoonToken)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype OutAndAway = OutAndAway ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outAndAway :: ActCard OutAndAway
outAndAway =
  act (3, A) OutAndAway Cards.outAndAway Nothing

instance HasAbilities OutAndAway where
  getAbilities (OutAndAway x) =
    [ mkAbility x 1 $ actionAbilityWithCost (HandDiscardCost 2 #any)
    , restricted x 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage OutAndAway where
  runMessage msg a@(OutAndAway attrs) = runQueueT $ case msg of
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
    _ -> OutAndAway <$> liftRunMessage msg attrs
