module Arkham.Treachery.Cards.ArcaneBarrier (ArcaneBarrier (..), arcaneBarrier) where

import Arkham.Cost
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ArcaneBarrier = ArcaneBarrier TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneBarrier :: TreacheryCard ArcaneBarrier
arcaneBarrier = treachery ArcaneBarrier Cards.arcaneBarrier

instance HasModifiersFor ArcaneBarrier where
  getModifiersFor target (ArcaneBarrier attrs) | treacheryOn attrs target = do
    pure $ toModifiers attrs [AdditionalCostToLeave $ SkillTestCost (toSource attrs) #willpower 4]
  getModifiersFor _ _ = pure []

instance RunMessage ArcaneBarrier where
  runMessage msg t@(ArcaneBarrier attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mLocation <- field InvestigatorLocation iid
      for_ mLocation $ attachTreachery attrs
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      let cancelMove = movementModifier attrs iid CannotMove
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
    _ -> ArcaneBarrier <$> lift (runMessage msg attrs)
