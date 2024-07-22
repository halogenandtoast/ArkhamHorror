module Arkham.Location.Cards.BoneFilledCaverns (
  boneFilledCaverns,
  BoneFilledCaverns (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.Draw.Types
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
  deriving anyclass (ToJSON, FromJSON)

newtype BoneFilledCaverns = BoneFilledCaverns (LocationAttrs `With` Metadata)
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boneFilledCaverns :: LocationCard BoneFilledCaverns
boneFilledCaverns =
  locationWith (BoneFilledCaverns . (`with` Metadata Nothing)) Cards.boneFilledCaverns 3 (PerPlayer 2)
    $ (connectsToL .~ adjacentLocations)
    . (costToEnterUnrevealedL .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation])

instance HasModifiersFor BoneFilledCaverns where
  getModifiersFor (InvestigatorTarget iid) (BoneFilledCaverns (attrs `With` metadata)) = do
    pure $ case affectedInvestigator metadata of
      Just iid' | iid == iid' -> toModifiers attrs [FewerSlots #hand 1]
      _ -> []
  getModifiersFor _ _ = pure []

instance HasAbilities BoneFilledCaverns where
  getAbilities (BoneFilledCaverns (attrs `With` _)) =
    withRevealedAbilities attrs
      $ [ restrictedAbility
            attrs
            1
            ( oneOf
                [ Negate (exists $ LocationInDirection dir (be attrs)) | dir <- [Below, RightOf]
                ]
            )
            $ forced (RevealLocation #when Anyone $ be attrs)
        ]

instance RunMessage BoneFilledCaverns where
  runMessage msg l@(BoneFilledCaverns (attrs `With` metadata)) = case msg of
    Investigate investigation | investigation.location == toId attrs && not investigation.isAction -> do
      result <- runMessage msg attrs
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
    _ -> BoneFilledCaverns . (`with` metadata) <$> runMessage msg attrs
