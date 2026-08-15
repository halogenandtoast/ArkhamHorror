module Arkham.Homebrew.DarkMatter.Assets.ErwinSimmonsFading (erwinSimmonsFading) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  FacedownEncounterCard (..),
  getFacedownEncounterCards,
 )
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

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
    [ controlled_ a 1
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
      unless (null investigators) do
        facedown <- concat <$> traverse getFacedownEncounterCards investigators
        unless (null facedown) do
          shuffled <- shuffle $ FacedownAsset attrs.id : facedown
          for_ (zip shuffled $ cycleN (length shuffled) investigators) \(card, iid) ->
            case card of
              FacedownTreachery tid -> place tid (FacedownInThreatArea iid)
              FacedownEnemy eid -> place eid (FacedownInThreatArea iid)
              FacedownAsset aid -> place aid (FacedownInThreatArea iid)
      pure a
    _ -> ErwinSimmonsFading <$> liftRunMessage msg attrs
