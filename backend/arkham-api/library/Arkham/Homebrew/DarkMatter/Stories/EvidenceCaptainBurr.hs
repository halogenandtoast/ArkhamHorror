module Arkham.Homebrew.DarkMatter.Stories.EvidenceCaptainBurr (evidenceCaptainBurr) where

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

"Captain Burr is not an imitation. Put this card into play next to the act deck.
Forced - After you draw the Captain Burr story asset from the scanning deck: Draw the
top card of the encounter deck."

The forced ability matches the narrow @scan[<card code>]@ window for that crew
asset, so it never triggers on any other scan.
-}
newtype EvidenceCaptainBurr = EvidenceCaptainBurr StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evidenceCaptainBurr :: StoryCard EvidenceCaptainBurr
evidenceCaptainBurr = story EvidenceCaptainBurr Cards.evidenceCaptainBurr

instance HasAbilities EvidenceCaptainBurr where
  getAbilities (EvidenceCaptainBurr a) =
    [mkAbility a 1 $ forced $ CampaignEvent #after Nothing (scanEventForCard Assets.captainBurr)]

instance RunMessage EvidenceCaptainBurr where
  runMessage msg s@(EvidenceCaptainBurr attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ StoryMessage $ PlaceStory (toCard attrs) NextToAct
      pure s
    UseCardAbility _ (isSource attrs -> True) 1 (getScanResult -> Just r) _ -> do
      drawEncounterCard (scannedBy r) (attrs.ability 1)
      pure s
    _ -> EvidenceCaptainBurr <$> liftRunMessage msg attrs
