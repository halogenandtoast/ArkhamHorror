module Arkham.Asset.Cards.Newspaper2 (newspaper2, Newspaper2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Projection

newtype Metadata = Metadata {active :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Newspaper2 = Newspaper2 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newspaper2 :: AssetCard Newspaper2
newspaper2 = asset (Newspaper2 . (`with` Metadata False)) Cards.newspaper2

instance HasModifiersFor Newspaper2 where
  getModifiersFor (InvestigatorTarget iid) (Newspaper2 (a `With` metadata)) | controlledBy a iid = do
    clueCount <- field InvestigatorClues iid
    pure
      $ toModifiers a
      $ guard (clueCount == 0)
      *> ActionSkillModifier #investigate #intellect 2
      : [DiscoveredClues 1 | active metadata]
  getModifiersFor _ _ = pure []

instance HasAbilities Newspaper2 where
  getAbilities (Newspaper2 (a `With` _)) =
    [ controlledAbility a 1 (youExist $ InvestigatorWithoutAnyClues <> at_ LocationWithAnyClues)
        $ freeReaction
        $ Matcher.DiscoverClues #when You YourLocation (atLeast 1)
    ]

instance RunMessage Newspaper2 where
  runMessage msg (Newspaper2 (attrs `With` metadata)) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure . Newspaper2 $ attrs `with` Metadata True
    Do (Msg.DiscoverClues {}) -> do
      pure . Newspaper2 $ attrs `with` Metadata False
    _ -> Newspaper2 . (`with` metadata) <$> runMessage msg attrs
