module Arkham.Homebrew.TheMasqueOfTheRedDeath.Agendas.UnderTheSkin (underTheSkin) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Investigator (getCanPlaceCluesOnLocationCount)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (addSkullEffectsToToken, scenarioI18n)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorName))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Name (toTitle)
import Arkham.Projection

newtype UnderTheSkin = UnderTheSkin AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underTheSkin :: AgendaCard UnderTheSkin
underTheSkin = agenda (2, A) UnderTheSkin Cards.underTheSkin (Static 5)

instance RunMessage UnderTheSkin where
  runMessage msg a@(UnderTheSkin attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      total <- perPlayer 2
      payers <-
        select UneliminatedInvestigator >>= traverse \iid -> do
          (iid,) <$> getCanPlaceCluesOnLocationCount iid
      let able = filter ((> 0) . snd) payers

      -- One decision, made once by the group.
      leadChooseOneM $ scenarioI18n $ scope "underTheSkin" $ countVar total do
        labeledValidate (notNull able) "placeClues" case able of
          -- Nobody else can contribute, so there is nothing to apportion.
          [(iid, cap)] -> placeCluesOnLocation iid attrs (min total cap)
          _ -> do
            lead <- getLead
            choices <- for able \(iid, cap) -> do
              name <- fieldMap InvestigatorName toTitle iid
              pure (name, (0, min total cap))
            let available = sum (map snd able)
            chooseAmounts lead "Clues to place" (TotalAmountTarget (min total available)) choices attrs
        labeled "revealTokens" $ eachInvestigator \iid -> requestChaosTokens iid attrs 1

      advanceAgendaDeck attrs
      pure a
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      withInvestigatorAmounts choices (`placeCluesOnLocation` attrs)
      pure a
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      continue_ iid
      faces <- getModifiedChaosTokenFaces tokens
      when (any (`elem` [#skull, #cultist, #tablet, #autofail]) faces)
        $ assignDamageAndHorror iid attrs 2 2
      pure a
    -- "Add each [skull] effect on your location to each [cultist] and [tablet]
    -- token you reveal during skill tests."
    ResolveChaosToken token face iid
      | onSide A attrs
      , face `elem` [#cultist, #tablet] -> do
          addSkullEffectsToToken iid token
          pure a
    _ -> UnderTheSkin <$> liftRunMessage msg attrs
