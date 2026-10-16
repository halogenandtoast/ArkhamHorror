module Arkham.Homebrew.TheLegendOfSleepyHollow.Acts.TheTaleOfIchabodCrane (
  theTaleOfIchabodCrane,
) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Placement

newtype TheTaleOfIchabodCrane = TheTaleOfIchabodCrane ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

{- | "Only investigators at Derelict Schoolhouse may spend the requisite number
of clues as a group to advance."
-}
theTaleOfIchabodCrane :: ActCard TheTaleOfIchabodCrane
theTaleOfIchabodCrane =
  act
    (1, A)
    TheTaleOfIchabodCrane
    Cards.theTaleOfIchabodCrane
    (Just $ GroupClueCost (PerPlayer 3) (locationIs Locations.derelictSchoolhouse))

instance RunMessage TheTaleOfIchabodCrane where
  runMessage msg a@(TheTaleOfIchabodCrane attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      schoolhouse <- selectJust $ locationIs Locations.derelictSchoolhouse
      createAssetAt_ Assets.ichabodCrane (AtLocation schoolhouse)
      createAssetAt_ Assets.ichabodsBooks (AtLocation schoolhouse)
      shuffleSetAsideIntoEncounterDeck
        $ cardsAre [Enemies.disembodiedSkull, Enemies.secretiveHag]
      advanceActDeck attrs
      pure a
    _ -> TheTaleOfIchabodCrane <$> liftRunMessage msg attrs
