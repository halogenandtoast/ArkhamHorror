module Arkham.Asset.Cards.Newspaper2
  ( newspaper2
  , Newspaper2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype Metadata = Metadata { active :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Newspaper2 = Newspaper2 (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newspaper2 :: AssetCard Newspaper2
newspaper2 = asset (Newspaper2 . (`with` Metadata False)) Cards.newspaper2

instance HasModifiersFor Newspaper2 where
  getModifiersFor (InvestigatorTarget iid) (Newspaper2 (a `With` metadata))
    | controlledBy a iid = do
      clueCount <- field InvestigatorClues iid
      pure
        $ toModifiers a
        $ [ ActionSkillModifier Action.Investigate SkillIntellect 2
          | clueCount == 0
          ]
        <> [ DiscoveredClues 1 | active metadata ]
  getModifiersFor _ _ = pure []

instance HasAbilities Newspaper2 where
  getAbilities (Newspaper2 (a `With` _)) =
    [ restrictedAbility a 1 (InvestigatorExists investigatorMatcher)
        $ flip ReactionAbility Free
        $ Matcher.DiscoverClues Timing.When You YourLocation
        $ AtLeast
        $ Static 1
    ]
   where
    investigatorMatcher =
      You <> InvestigatorWithoutAnyClues <> InvestigatorAt LocationWithAnyClues

instance RunMessage Newspaper2 where
  runMessage msg (Newspaper2 (attrs `With` metadata)) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source ->
      pure . Newspaper2 $ attrs `with` Metadata True
    DiscoverCluesAtLocation{} -> do
      pure . Newspaper2 $ attrs `with` Metadata False
    _ -> Newspaper2 . (`with` metadata) <$> runMessage msg attrs
