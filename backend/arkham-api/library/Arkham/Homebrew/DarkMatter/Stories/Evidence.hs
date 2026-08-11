module Arkham.Homebrew.DarkMatter.Stories.Evidence (
  evidenceAdamTanner,
  evidenceCaptainBurr,
  evidenceDoctorFeng,
  evidenceLtArcherMichaels,
  evidenceMUD12Mudbug,
  evidenceSophie,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (ScanResult (..), scanEvent)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Story.Import.Lifted
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

{- | The six "Evidence" cards clear one crew member of being an imitation. They
share one rules text, differing only in which crew asset they vouch for:

"<name> is not an imitation. Put this card into play next to the act deck.
Forced - After you draw the <name> story asset from the scanning deck: Draw the
top card of the encounter deck."
-}
newtype Evidence = Evidence StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkEvidence :: CardDef -> StoryCard Evidence
mkEvidence = story Evidence

evidenceAdamTanner :: StoryCard Evidence
evidenceAdamTanner = mkEvidence Cards.evidenceAdamTanner

evidenceCaptainBurr :: StoryCard Evidence
evidenceCaptainBurr = mkEvidence Cards.evidenceCaptainBurr

evidenceDoctorFeng :: StoryCard Evidence
evidenceDoctorFeng = mkEvidence Cards.evidenceDoctorFeng

evidenceLtArcherMichaels :: StoryCard Evidence
evidenceLtArcherMichaels = mkEvidence Cards.evidenceLtArcherMichaels

evidenceMUD12Mudbug :: StoryCard Evidence
evidenceMUD12Mudbug = mkEvidence Cards.evidenceMUD12Mudbug

evidenceSophie :: StoryCard Evidence
evidenceSophie = mkEvidence Cards.evidenceSophie

-- | The crew story asset this Evidence card vouches for.
clearedCrewMember :: StoryAttrs -> CardDef
clearedCrewMember a
  | isEvidenceFor Cards.evidenceCaptainBurr = Assets.captainBurr
  | isEvidenceFor Cards.evidenceDoctorFeng = Assets.doctorFeng
  | isEvidenceFor Cards.evidenceLtArcherMichaels = Assets.ltArcherMichaels
  | isEvidenceFor Cards.evidenceMUD12Mudbug = Assets.muD12Mudbug
  | isEvidenceFor Cards.evidenceSophie = Assets.sophie
  | otherwise = Assets.adamTanner
 where
  isEvidenceFor def = toCardCode (toCardDef a) == toCardCode def

instance HasAbilities Evidence where
  getAbilities (Evidence a) =
    [mkAbility a 1 $ forced $ ScenarioEvent #after Nothing scanEvent]

getScanResult :: [Window] -> Maybe ScanResult
getScanResult = \case
  (windowType -> Window.ScenarioEvent key _ v) : _ | key == scanEvent -> Just (toResult v)
  _ : rest -> getScanResult rest
  [] -> Nothing

instance RunMessage Evidence where
  runMessage msg s@(Evidence attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ StoryMessage $ PlaceStory (toCard attrs) NextToAct
      pure s
    UseCardAbility _ (isSource attrs -> True) 1 (getScanResult -> Just r) _
      | maybe False ((== toCardCode (clearedCrewMember attrs)) . toCardCode) (scannedCard r) -> do
          drawEncounterCard (scannedBy r) (attrs.ability 1)
          pure s
    _ -> Evidence <$> liftRunMessage msg attrs
