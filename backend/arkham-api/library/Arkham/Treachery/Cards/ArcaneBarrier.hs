module Arkham.Treachery.Cards.ArcaneBarrier (ArcaneBarrier (..), arcaneBarrier) where

import Arkham.Cost
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types
import Arkham.Placement
import Arkham.Projection
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
      mLocation <- field InvestigatorLocation iid
      for_ mLocation $ attachTreachery attrs
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      cancelMove <- movementModifier attrs iid CannotMove
      chooseOne
        iid
        [ Label "Cancel Move" [cancelMove]
        , Label
            "Discard top 5 cards of your deck"
            [DiscardTopOfDeck iid 5 (toSource attrs) Nothing]
        ]
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid attrs attrs
      pure t
    _ -> ArcaneBarrier <$> liftRunMessage msg attrs
