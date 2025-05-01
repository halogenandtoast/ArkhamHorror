module Arkham.Treachery.Cards.ArcaneBarrier (arcaneBarrier) where

import Arkham.Cost
import Arkham.I18n
import Arkham.Helpers.Modifiers
import Arkham.Message.Lifted.Choose
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (movementModifier)

newtype ArcaneBarrier = ArcaneBarrier TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneBarrier :: TreacheryCard ArcaneBarrier
arcaneBarrier = treachery ArcaneBarrier Cards.arcaneBarrier

instance HasModifiersFor ArcaneBarrier where
  getModifiersFor (ArcaneBarrier attrs) = case attrs.placement of
    AttachedToLocation lid ->
      modified_
        attrs
        lid
        [ AdditionalCostToLeave $ SkillTestCost (toSource attrs) #willpower (Fixed 4)
        , AdditionalCostToEnter $ SkillTestCost (toSource attrs) #willpower (Fixed 4)
        ]
    _ -> pure mempty

instance RunMessage ArcaneBarrier where
  runMessage msg t@(ArcaneBarrier attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid (attachTreachery attrs)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> withI18n do
      chooseOneM iid do
        labeled' "cancelMove" $ cancelMovement attrs iid
        countVar 5 $ labeled' "discardTopOfYourDeck" $ discardTopOfDeck iid attrs 5
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid attrs attrs
      pure t
    _ -> ArcaneBarrier <$> liftRunMessage msg attrs
