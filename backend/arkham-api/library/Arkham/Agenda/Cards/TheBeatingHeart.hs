module Arkham.Agenda.Cards.TheBeatingHeart (theBeatingHeart) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Classes.HasGame
import Arkham.Helpers.Text
import Arkham.I18n
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Scenarios.TheHeartOfMadness.Helpers

newtype TheBeatingHeart = TheBeatingHeart AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities TheBeatingHeart where
  getAbilities (TheBeatingHeart a) = [restricted a 1 ElectrostaticDetonation $ forced AnyWindow]

theBeatingHeart :: AgendaCard TheBeatingHeart
theBeatingHeart = agenda (1, A) TheBeatingHeart Cards.theBeatingHeart (Static 5)

partnerScope :: HasCardCode a => a -> Text
partnerScope x
  | isCardCode x Assets.drAmyKenslerProfessorOfBiology = "kensler"
  | isCardCode x Assets.professorWilliamDyerProfessorOfGeology = "dyer"
  | isCardCode x Assets.danforthBrilliantStudent = "danforth"
  | isCardCode x Assets.averyClaypoolAntarcticGuide = "claypool"
  | isCardCode x Assets.takadaHirokoAeroplaneMechanic = "takada"
  | isCardCode x Assets.roaldEllsworthIntrepidExplorer = "ellsworth"
  | isCardCode x Assets.eliyahAshevakDogHandler = "ashevak"
  | isCardCode x Assets.drMalaSinhaDaringPhysician = "sinha"
  | isCardCode x Assets.jamesCookieFredericksDubiousChoice = "cookie"
  | otherwise = error "not implemented"

isSaved :: (HasCardCode a, HasGame m) => a -> m Bool
isSaved x = getPartnerIsAlive savingPartner
 where
  savingPartner
    | isCardCode x Assets.drAmyKenslerProfessorOfBiology = Assets.drMalaSinhaDaringPhysician
    | isCardCode x Assets.professorWilliamDyerProfessorOfGeology = Assets.drAmyKenslerProfessorOfBiology
    | isCardCode x Assets.danforthBrilliantStudent = Assets.professorWilliamDyerProfessorOfGeology
    | isCardCode x Assets.averyClaypoolAntarcticGuide = Assets.roaldEllsworthIntrepidExplorer
    | isCardCode x Assets.takadaHirokoAeroplaneMechanic = Assets.eliyahAshevakDogHandler
    | isCardCode x Assets.roaldEllsworthIntrepidExplorer = Assets.jamesCookieFredericksDubiousChoice
    | isCardCode x Assets.eliyahAshevakDogHandler = Assets.averyClaypoolAntarcticGuide
    | isCardCode x Assets.drMalaSinhaDaringPhysician = Assets.danforthBrilliantStudent
    | isCardCode x Assets.jamesCookieFredericksDubiousChoice = Assets.takadaHirokoAeroplaneMechanic
    | otherwise = error "not implemented"

instance RunMessage TheBeatingHeart where
  runMessage msg a@(TheBeatingHeart attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> scenarioI18n 1 $ scope "interlude" do
      partners <- getPartnersWithStatus (== Safe)
      case nonEmpty partners of
        Nothing -> story $ i18n "instructions"
        Just ps -> do
          x <- sample ps
          story $ i18n "instructions" <> i18n "part1"
          scope (partnerScope x) do
            saved <- isSaved x
            storyWithCard (toCardDef x)
              $ blueFlavor
              $ validatedEntry "victim"
              <> validateEntry saved "saved"
              <> validateEntry (not saved) "otherwise"

            unless saved do
              setPartnerStatus x Eliminated
              selectForMaybeM (assetIs x.cardCode) (push . AssetDefeated (toSource attrs))
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push R3
      pure a
    _ -> TheBeatingHeart <$> liftRunMessage msg attrs
