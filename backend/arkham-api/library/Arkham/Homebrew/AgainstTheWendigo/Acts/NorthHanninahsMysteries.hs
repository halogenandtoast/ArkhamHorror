module Arkham.Homebrew.AgainstTheWendigo.Acts.NorthHanninahsMysteries (
  northHanninahsMysteries,
) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Civilized)
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Resolution

newtype NorthHanninahsMysteries = NorthHanninahsMysteries ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northHanninahsMysteries :: ActCard NorthHanninahsMysteries
northHanninahsMysteries = act (3, A) NorthHanninahsMysteries Cards.northHanninahsMysteries Nothing

instance HasAbilities NorthHanninahsMysteries where
  getAbilities = actAbilities1 \x ->
    -- "Objective - If all investigators who have not resigned or been defeated
    -- are in a Civilized location you can advance this act at any time."
    restricted x 1 (notExists $ UneliminatedInvestigator <> not_ (InvestigatorAt $ LocationWithTrait Civilized))
      $ Objective freeTrigger_

instance RunMessage NorthHanninahsMysteries where
  runMessage msg a@(NorthHanninahsMysteries attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      record YouHaveEnoughEvidenceToClearDrNadelmann
      push $ ScenarioResolution $ Resolution 2
      pure a
    _ -> NorthHanninahsMysteries <$> liftRunMessage msg attrs
