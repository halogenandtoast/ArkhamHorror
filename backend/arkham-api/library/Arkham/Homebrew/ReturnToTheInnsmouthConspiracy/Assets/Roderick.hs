module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Assets.Roderick (roderick) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Assets.Hybrid
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Assets qualified as Cards

newtype Roderick = Roderick AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Activating the {fast} ability on the act costs 1 fewer clues (to a minimum of 1
clue)." The act's ability is an X cost paid in clues, so the discount is applied where
the act turns clues paid into cards revealed, rather than as a cost modifier here --
see 'Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Acts.TheSearchForAgentHarperV2'.
-}
roderick :: AssetCard Roderick
roderick = ally Roderick Cards.roderick (0, 1)

instance HasAbilities Roderick where
  getAbilities (Roderick a) = [removeFromGameWhenDefeated a 1]

instance RunMessage Roderick where
  runMessage msg a@(Roderick attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putHybridIntoPlay iid attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeFromGame attrs
      pure a
    _ -> Roderick <$> liftRunMessage msg attrs
