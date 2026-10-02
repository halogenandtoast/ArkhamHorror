module Arkham.Homebrew.DarkMatter.Stories.AWebbOfDiscovery (aWebbOfDiscovery) where

import Arkham.Card (toCard)
import Arkham.Helpers.Modifiers (ModifierType (Blank), modified_)
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scienceCardsControlledOrOwnedBy)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype AWebbOfDiscovery = AWebbOfDiscovery StoryAttrs
  deriving anyclass (IsStory, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aWebbOfDiscovery :: StoryCard AWebbOfDiscovery
aWebbOfDiscovery = persistStory $ story AWebbOfDiscovery Cards.aWebbOfDiscovery

{- | "You may treat the text box of the attached location as if it were blank."
The option is not modelled: the blank is always on while this is attached.
-}
instance HasModifiersFor AWebbOfDiscovery where
  getModifiersFor (AWebbOfDiscovery a) = case a.placement of
    AtLocation lid -> modified_ a lid [Blank]
    _ -> pure ()

{- | "If an investigator at your location controls or owns a Science card, attach
this card to any location. Otherwise, remove this card from the game."
-}
instance RunMessage AWebbOfDiscovery where
  runMessage msg s@(AWebbOfDiscovery attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      colocated <- select $ colocatedWith iid
      hasScience <- anyM (fmap (> 0) . scienceCardsControlledOrOwnedBy) colocated
      if hasScience
        then do
          locations <- select Anywhere
          chooseTargetM iid locations
            $ push
            . StoryMessage
            . PlaceStory (toCard attrs)
            . AtLocation
        else removeStory attrs
      pure s
    _ -> AWebbOfDiscovery <$> liftRunMessage msg attrs
