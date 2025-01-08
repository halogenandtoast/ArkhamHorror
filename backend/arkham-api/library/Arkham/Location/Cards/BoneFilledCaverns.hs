module Arkham.Location.Cards.BoneFilledCaverns (boneFilledCaverns, BoneFilledCaverns (..)) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Id
import Arkham.Investigate ()
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype Metadata = Metadata {affectedInvestigator :: Maybe InvestigatorId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BoneFilledCaverns = BoneFilledCaverns (LocationAttrs `With` Metadata)
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boneFilledCaverns :: LocationCard BoneFilledCaverns
boneFilledCaverns =
  locationWith (BoneFilledCaverns . (`with` Metadata Nothing)) Cards.boneFilledCaverns 3 (PerPlayer 2)
    $ (connectsToL .~ adjacentLocations)
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasModifiersFor BoneFilledCaverns where
  getModifiersFor (BoneFilledCaverns (a `With` metadata)) = do
    case affectedInvestigator metadata of
      Nothing -> pure mempty
      Just iid -> modified_ a iid [FewerSlots #hand 1]

instance HasAbilities BoneFilledCaverns where
  getAbilities (BoneFilledCaverns (a `With` _)) =
    extendRevealed1 a
      $ restricted a 1 (oneOf [notExists $ LocationInDirection dir (be a) | dir <- [Below, RightOf]])
      $ forced (RevealLocation #when Anyone $ be a)

instance RunMessage BoneFilledCaverns where
  runMessage msg l@(BoneFilledCaverns (attrs `With` metadata)) = runQueueT $ case msg of
    Investigate investigation | investigation.location == toId attrs && not investigation.isAction -> do
      result <- liftRunMessage msg attrs
      push $ RefillSlots investigation.investigator
      pure $ BoneFilledCaverns $ With result (Metadata $ Just investigation.investigator)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- countM (directionEmpty attrs) [Below, RightOf]
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck n
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placeDrawnLocations attrs drewCards.cards [Below, RightOf]
      pure l
    SkillTestEnds _ _ _ -> pure $ BoneFilledCaverns $ With attrs (Metadata Nothing)
    _ -> BoneFilledCaverns . (`with` metadata) <$> liftRunMessage msg attrs
