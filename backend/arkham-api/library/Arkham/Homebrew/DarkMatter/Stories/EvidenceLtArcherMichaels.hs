module Arkham.Homebrew.DarkMatter.Stories.EvidenceLtArcherMichaels (evidenceLtArcherMichaels) where

import Arkham.Ability
import Arkham.Card (toCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (ScanResult (..), getScanResult, scanEventForCard)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Import.Lifted

{- | One of the six "Evidence" cards, each clearing one crew member of being an
imitation:

"Lt. "Archer" Michaels is not an imitation. Put this card into play next to the act deck.
Forced - After you draw the Lt. "Archer" Michaels story asset from the scanning deck: Draw the
top card of the encounter deck."

The forced ability matches the narrow @scan[<card code>]@ window for that crew
asset, so it never triggers on any other scan.
-}
newtype EvidenceLtArcherMichaels = EvidenceLtArcherMichaels StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evidenceLtArcherMichaels :: StoryCard EvidenceLtArcherMichaels
evidenceLtArcherMichaels = story EvidenceLtArcherMichaels Cards.evidenceLtArcherMichaels

instance HasAbilities EvidenceLtArcherMichaels where
  getAbilities (EvidenceLtArcherMichaels a) =
    [mkAbility a 1 $ forced $ CampaignEvent #after Nothing (scanEventForCard Assets.ltArcherMichaels)]

instance RunMessage EvidenceLtArcherMichaels where
  runMessage msg s@(EvidenceLtArcherMichaels attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ StoryMessage $ PlaceStory (toCard attrs) NextToAct
      pure s
    UseCardAbility _ (isSource attrs -> True) 1 (getScanResult -> Just r) _ -> do
      drawEncounterCard (scannedBy r) (attrs.ability 1)
      pure s
    _ -> EvidenceLtArcherMichaels <$> liftRunMessage msg attrs
