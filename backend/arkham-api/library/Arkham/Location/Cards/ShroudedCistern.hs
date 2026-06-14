module Arkham.Location.Cards.ShroudedCistern (shroudedCistern) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (increaseThisFloodLevel)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheGrandVault.Helpers (activateLocation)
import Arkham.Trait (Trait (Vault))

newtype ShroudedCistern = ShroudedCistern LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shroudedCistern :: LocationCard ShroudedCistern
shroudedCistern = location ShroudedCistern Cards.shroudedCistern 3 (Static 2)

instance HasAbilities ShroudedCistern where
  getAbilities (ShroudedCistern a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ Matcher.RevealLocation #after Anyone (be a)
      , restricted a 2 Here actionAbility
      ]

instance RunMessage ShroudedCistern where
  runMessage msg l@(ShroudedCistern attrs) = runQueueT $ case msg of
    -- "This location's flood level cannot be decreased." Suppress any attempt to
    -- decrease the flood level on this location by not delegating to the runner.
    DecreaseFloodLevel lid | lid == attrs.id -> pure l
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      increaseThisFloodLevel attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      activateLocation (attrs.ability 2) attrs.id
      locations <- select $ LocationWithTrait Vault <> CanHaveFloodLevelIncreased
      chooseTargetM iid locations \lid -> increaseThisFloodLevel lid
      pure l
    _ -> ShroudedCistern <$> liftRunMessage msg attrs
