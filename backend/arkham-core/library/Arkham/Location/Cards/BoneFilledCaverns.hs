module Arkham.Location.Cards.BoneFilledCaverns (
  boneFilledCaverns,
  BoneFilledCaverns (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Id
import Arkham.Investigate ()
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype Metadata = Metadata {affectedInvestigator :: Maybe InvestigatorId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks)

newtype BoneFilledCaverns = BoneFilledCaverns (LocationAttrs `With` Metadata)
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

boneFilledCaverns :: LocationCard BoneFilledCaverns
boneFilledCaverns =
  locationWith (BoneFilledCaverns . (`with` Metadata Nothing)) Cards.boneFilledCaverns 3 (PerPlayer 2)
    $ (connectsToL .~ adjacentLocations)
    . (costToEnterUnrevealedL .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation])

instance HasModifiersFor BoneFilledCaverns where
  getModifiersFor (InvestigatorTarget iid) (BoneFilledCaverns (attrs `With` metadata)) = do
    case affectedInvestigator metadata of
      Just iid' | iid == iid' -> do
        pure $ toModifiers attrs [FewerSlots #hand 1]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities BoneFilledCaverns where
  getAbilities (BoneFilledCaverns (attrs `With` _)) =
    withRevealedAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            ( AnyCriterion
                [ Negate (exists $ LocationInDirection dir (LocationWithId $ toId attrs)) | dir <- [Below, RightOf]
                ]
            )
            $ ForcedAbility (RevealLocation #when Anyone $ LocationWithId $ toId attrs)
        ]

instance RunMessage BoneFilledCaverns where
  runMessage msg l@(BoneFilledCaverns (attrs `With` metadata)) = case msg of
    Investigate investigation | investigation.location == toId attrs && not investigation.isAction -> do
      result <- runMessage msg attrs
      push $ RefillSlots investigation.investigator
      pure $ BoneFilledCaverns $ With result (Metadata $ Just investigation.investigator)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- countM (directionEmpty attrs) [Below, RightOf]
      push $ DrawFromScenarioDeck iid CatacombsDeck (toTarget attrs) n
      pure l
    DrewFromScenarioDeck _ _ (isTarget attrs -> True) cards -> do
      placeDrawnLocations attrs cards [Below, RightOf]
      pure l
    SkillTestEnds _ _ -> pure $ BoneFilledCaverns $ With attrs (Metadata Nothing)
    _ -> BoneFilledCaverns . (`with` metadata) <$> runMessage msg attrs
