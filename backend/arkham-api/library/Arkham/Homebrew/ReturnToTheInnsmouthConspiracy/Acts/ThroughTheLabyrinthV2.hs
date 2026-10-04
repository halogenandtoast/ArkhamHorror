module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Acts.ThroughTheLabyrinthV2 (throughTheLabyrinthV2) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as HBTreacheries
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (deepOneInvestigator, scenarioI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Scenarios.TheInnsmouthConspiracy.InTooDeep.Helpers hiding (scenarioI18n)

newtype ThroughTheLabyrinthV2 = ThroughTheLabyrinthV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throughTheLabyrinthV2 :: ActCard ThroughTheLabyrinthV2
throughTheLabyrinthV2 = act (1, A) ThroughTheLabyrinthV2 Cards.throughTheLabyrinthV2 Nothing

instance HasAbilities ThroughTheLabyrinthV2 where
  getAbilities (ThroughTheLabyrinthV2 a) =
    extend
      a
      [ restrictedAbility a 1 (exists $ YourLocation <> LocationWithAdjacentBarrier)
          $ FastAbility (GroupClueCost (StaticWithPerPlayer 1 1) Anywhere)
      , onlyOnce $ restrictedAbility a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
      ]

{- | Differs from Through the Labyrinth (07128) only on the back: a Deep One investigator
leaves town carrying Innsmouth Influence with them.
-}
instance RunMessage ThroughTheLabyrinthV2 where
  runMessage msg a@(ThroughTheLabyrinthV2 attrs) = runQueueT $ scenarioI18n "returnToInTooDeep" $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid (removeBarrierBetweenConnected iid)
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      record TheInvestigatorsMadeItSafelyToTheirVehicles
      deepOnes <- select deepOneInvestigator
      unless (null deepOnes) do
        flavor $ scope "resolution" $ p "innsmouthInfluence"
        for_ deepOnes \iid -> addCampaignCardToDeck iid ShuffleIn HBTreacheries.innsmouthInfluence
      push R1
      pure a
    _ -> ThroughTheLabyrinthV2 <$> liftRunMessage msg attrs
