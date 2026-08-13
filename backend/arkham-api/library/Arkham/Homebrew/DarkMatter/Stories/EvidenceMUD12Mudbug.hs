module Arkham.Homebrew.DarkMatter.Stories.EvidenceMUD12Mudbug (evidenceMUD12Mudbug) where

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

"MU-D12 "Mudbug" is not an imitation. Put this card into play next to the act deck.
Forced - After you draw the MU-D12 "Mudbug" story asset from the scanning deck: Draw the
top card of the encounter deck."

The forced ability matches the narrow @scan[<card code>]@ window for that crew
asset, so it never triggers on any other scan.
-}
newtype EvidenceMUD12Mudbug = EvidenceMUD12Mudbug StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evidenceMUD12Mudbug :: StoryCard EvidenceMUD12Mudbug
evidenceMUD12Mudbug = story EvidenceMUD12Mudbug Cards.evidenceMUD12Mudbug

instance HasAbilities EvidenceMUD12Mudbug where
  getAbilities (EvidenceMUD12Mudbug a) =
    [mkAbility a 1 $ forced $ CampaignEvent #after Nothing (scanEventForCard Assets.muD12Mudbug)]

instance RunMessage EvidenceMUD12Mudbug where
  runMessage msg s@(EvidenceMUD12Mudbug attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ StoryMessage $ PlaceStory (toCard attrs) NextToAct
      pure s
    UseCardAbility _ (isSource attrs -> True) 1 (getScanResult -> Just r) _ -> do
      drawEncounterCard (scannedBy r) (attrs.ability 1)
      pure s
    _ -> EvidenceMUD12Mudbug <$> liftRunMessage msg attrs
