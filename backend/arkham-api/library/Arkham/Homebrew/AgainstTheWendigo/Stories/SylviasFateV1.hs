module Arkham.Homebrew.AgainstTheWendigo.Stories.SylviasFateV1 (sylviasFateV1) where

import Arkham.Ability
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Story.Import.Lifted hiding (DiscoverClues)

newtype SylviasFateV1 = SylviasFateV1 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | See 'Arkham.Homebrew.AgainstTheWendigo.Stories.BernardsFateV1'. Here the
-- forest itself took Sylvia; the card flips into The Heart of the Forest, whose
-- revelation drags everyone in the Impenetrable Forest into it.
sylviasFateV1 :: StoryCard SylviasFateV1
sylviasFateV1 = persistStory $ story SylviasFateV1 Cards.sylviasFateV1

instance HasAbilities SylviasFateV1 where
  getAbilities (SylviasFateV1 a) =
    [ restricted a 1 (notExists $ locationIs Locations.impenetrableForest <> LocationWithAnyClues)
        $ forced
        $ DiscoverClues #after Anyone (locationIs Locations.impenetrableForest) AnyValue
    ]

instance RunMessage SylviasFateV1 where
  runMessage msg s@(SylviasFateV1 attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.impenetrableForest) reveal
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      record YouHaveDiscoveredSylviasFate
      reveal =<< placeLocationCard Locations.theHeartOfTheForest
      removeStory attrs
      pure s
    _ -> SylviasFateV1 <$> liftRunMessage msg attrs
