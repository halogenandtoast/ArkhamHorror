module Arkham.Act.Cards.IntoTheBeyond (IntoTheBeyond (..), intoTheBeyond) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype IntoTheBeyond = IntoTheBeyond ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheBeyond :: ActCard IntoTheBeyond
intoTheBeyond = act (2, A) IntoTheBeyond Cards.intoTheBeyond Nothing

instance HasAbilities IntoTheBeyond where
  getAbilities (IntoTheBeyond x) =
    extend
      x
      [ mkAbility x 1 actionAbility
      , mkAbility x 2 $ Objective $ forced $ Enters #when Anyone "The Edge of the Universe"
      ]

instance RunMessage IntoTheBeyond where
  runMessage msg a@(IntoTheBeyond attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DiscardTopOfEncounterDeck iid 3 (toSource attrs) (Just $ toTarget attrs)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    DiscardedTopOfEncounterDeck iid cards _ target | isTarget attrs target -> do
      let locationCards = map toCard $ filterLocations cards
      focusCards locationCards \unfocus -> do
        chooseTargetM iid locationCards (push . ResolveRevelation iid)
        push unfocus
      pure a
    _ -> IntoTheBeyond <$> liftRunMessage msg attrs
