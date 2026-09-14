module Arkham.Homebrew.DarkMatter.Assets.ErwinSimmonsFading (erwinSimmonsFading) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  FacedownEncounterCard (FacedownAsset),
  anyFacedownEncounterCards,
  getFacedownEncounterCards,
  placeFacedownEncounterCardsEvenly,
 )
import Arkham.Matcher

newtype ErwinSimmonsFading = ErwinSimmonsFading AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erwinSimmonsFading :: AssetCard ErwinSimmonsFading
erwinSimmonsFading = asset ErwinSimmonsFading Cards.erwinSimmonsFading

{- | "Forced - At the end of the round or when you would resign, if there are
face-down encounter cards in any investigator's threat area: Shuffle Erwin
Simmons with all face-down encounter cards and distribute them back into each
investigator's threat area, as evenly as possible."
-}
instance HasAbilities ErwinSimmonsFading where
  getAbilities (ErwinSimmonsFading a) =
    [ restricted a 1 (ControlsThis <> anyFacedownEncounterCards)
        $ forced
        $ oneOf [RoundEnds #when, InvestigatorResigned #when You]
    ]

instance RunMessage ErwinSimmonsFading where
  runMessage msg a@(ErwinSimmonsFading attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select UneliminatedInvestigator
      facedown <- concatMapM getFacedownEncounterCards investigators
      unless (null facedown)
        $ placeFacedownEncounterCardsEvenly investigators (FacedownAsset attrs.id : facedown)
      pure a
    _ -> ErwinSimmonsFading <$> liftRunMessage msg attrs
