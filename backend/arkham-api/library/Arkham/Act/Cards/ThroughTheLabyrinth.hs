module Arkham.Act.Cards.ThroughTheLabyrinth (ThroughTheLabyrinth (..), throughTheLabyrinth) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Matcher
import Arkham.Scenarios.InTooDeep.Helpers

newtype ThroughTheLabyrinth = ThroughTheLabyrinth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throughTheLabyrinth :: ActCard ThroughTheLabyrinth
throughTheLabyrinth = act (1, A) ThroughTheLabyrinth Cards.throughTheLabyrinth Nothing

instance HasAbilities ThroughTheLabyrinth where
  getAbilities (ThroughTheLabyrinth a) =
    extend
      a
      [ restrictedAbility a 1 (exists $ YourLocation <> LocationWithAdjacentBarrier)
          $ FastAbility (GroupClueCost (StaticWithPerPlayer 1 1) Anywhere)
      , restrictedAbility a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
      ]

instance RunMessage ThroughTheLabyrinth where
  runMessage msg a@(ThroughTheLabyrinth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid (removeBarrierBetweenConnected iid)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      record TheInvestigatorsMadeItSafelyToTheirVehicles
      push R1
      pure a
    _ -> ThroughTheLabyrinth <$> liftRunMessage msg attrs
