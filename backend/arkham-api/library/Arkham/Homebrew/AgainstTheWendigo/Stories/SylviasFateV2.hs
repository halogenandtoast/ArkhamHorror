module Arkham.Homebrew.AgainstTheWendigo.Stories.SylviasFateV2 (sylviasFateV2) where

import Arkham.Ability
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Story.Import.Lifted hiding (DiscoverClues)

newtype SylviasFateV2 = SylviasFateV2 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'. Here Sylvia
-- came back as something that walks; the card flips into her.
sylviasFateV2 :: StoryCard SylviasFateV2
sylviasFateV2 = persistStory $ story SylviasFateV2 Cards.sylviasFateV2

instance HasAbilities SylviasFateV2 where
  getAbilities (SylviasFateV2 a) =
    [ restricted a 1 (notExists $ locationIs Locations.impenetrableForest <> LocationWithAnyClues)
        $ forced
        $ DiscoverClues #after Anyone (locationIs Locations.impenetrableForest) AnyValue
    ]

instance RunMessage SylviasFateV2 where
  runMessage msg s@(SylviasFateV2 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.impenetrableForest) reveal
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      record YouHaveDiscoveredSylviasFate
      createEnemyAtLocationMatching_
        Enemies.sylviaDavidson
        (locationIs Locations.impenetrableForest)
      removeStory attrs
      pure s
    _ -> SylviasFateV2 <$> liftRunMessage msg attrs
