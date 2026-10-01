module Arkham.Homebrew.CircusExMortis.Acts.ImpendingZenith (impendingZenith) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Act.Types (Field (ActClues))
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.Helpers (scenarioI18n)
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues, InvestigatorName))
import Arkham.Matcher
import Arkham.Name (toTitle)
import Arkham.Projection
import Arkham.Question (AmountTarget (MaxAmountTarget))

newtype ImpendingZenith = ImpendingZenith ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

impendingZenith :: ActCard ImpendingZenith
impendingZenith = act (2, A) ImpendingZenith Cards.impendingZenith Nothing

ritualClearing :: LocationMatcher
ritualClearing = locationIs Locations.ritualClearing

instance HasAbilities ImpendingZenith where
  getAbilities = actAbilities \x ->
    [ -- The only thing the action does is move clues onto the act, so it is not
      -- offered unless somebody at Ritual Clearing has a clue to move.
      restricted
        x
        1
        ( youExist (at_ ritualClearing)
            <> exists (InvestigatorAt ritualClearing <> InvestigatorWithAnyClues)
        )
        actionAbility
    , restricted x 2 (HasCalculation (ActFieldCalculation x.id ActClues) (AtLeast $ PerPlayer 2))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage ImpendingZenith where
  runMessage msg a@(ImpendingZenith attrs) = runQueueT $ scenarioI18n "redSunrise" $ scope "impendingZenith" $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Reveal 5 random chaos tokens." requestChaosTokens returns them afterwards.
      requestChaosTokens iid (attrs.ability 1) 5
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      -- "Investigators as a group at your location may move 1 clue they control to
      -- this act, plus 1 additional clue for each [moon] token you reveal." The
      -- group allowance is one per investigator here plus one per moon; it is a
      -- "may", so a total of 0 is a legal answer.
      moons <- count (== MoonToken) <$> getModifiedChaosTokenFaces tokens
      here <- selectCount $ InvestigatorAt (locationWithInvestigator iid)
      holders <-
        selectWithField InvestigatorClues
          $ InvestigatorAt (locationWithInvestigator iid)
          <> InvestigatorWithAnyClues
      let allowance = here + moons
      unless (null holders) do
        named <- for holders \(holder, clues) -> (,clues) <$> field InvestigatorName holder
        chooseAmounts
          iid
          (ikey' "label.moveClues")
          (MaxAmountTarget allowance)
          (map (\(name, clues) -> (toTitle name, (0, min clues allowance))) named)
          attrs
      pure a
    ResolveAmounts iid choices (isTarget attrs -> True) -> do
      named <- selectWithField InvestigatorName $ InvestigatorAt (locationWithInvestigator iid)
      for_ named \(holder, name) ->
        moveTokens (attrs.ability 1) holder attrs #clue (getChoiceAmount (toTitle name) choices)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> ImpendingZenith <$> liftRunMessage msg attrs
