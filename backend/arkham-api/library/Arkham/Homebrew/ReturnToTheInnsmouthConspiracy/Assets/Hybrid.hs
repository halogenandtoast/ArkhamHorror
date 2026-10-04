{- | Shared bits of the three Hybrid story allies from Return to The Vanishing of Elina
Harper. All three are shuffled into the Leads deck, so each is drawn as an encounter
card and enters play from its own Revelation, and each is removed from the game rather
than discarded when defeated.
-}
module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Assets.Hybrid (
  putHybridIntoPlay,
  removeFromGameWhenDefeated,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Card
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

-- | "Revelation - Put <ally> into play under any investigator's control."
putHybridIntoPlay :: ReverseQueue m => InvestigatorId -> AssetAttrs -> m ()
putHybridIntoPlay iid attrs = do
  investigators <- getInvestigators
  chooseOrRunOneM iid $ targets investigators \owner ->
    push $ TakeControlOfSetAsideAsset owner (toCard attrs)

-- | "Forced - When <ally> is defeated: Remove them from the game."
removeFromGameWhenDefeated :: AssetAttrs -> Int -> Ability
removeFromGameWhenDefeated a n = mkAbility a n $ forced $ AssetDefeated #when ByAny (be a)
