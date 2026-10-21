module Arkham.Act.Cards.TracesToNowhere.TakingOverTheSearch (takingOverTheSearch) where

import Arkham.Ability
import Arkham.Act.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Enemies
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Trait (Trait (Witness))

newtype TakingOverTheSearch = TakingOverTheSearch ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

takingOverTheSearch :: ActCard TakingOverTheSearch
takingOverTheSearch = act (1, A) TakingOverTheSearch Cards.takingOverTheSearch Nothing

instance HasAbilities TakingOverTheSearch where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (notExists (AssetWithTrait Witness) <> not_ (ScenarioDeckWithCard LeadsDeck))
        $ forced
        $ RoundEnds #when
    , restricted a 2 (ChaosTokenCountIs (SealedOnScenarioReference AnyChaosToken) (atLeast 1))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage TakingOverTheSearch where
  runMessage msg a@(TakingOverTheSearch attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator $ investigatorDefeated attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == attrs.id && onSide B attrs -> do
      sent <- sendAbductorToAWitness attrs
      unless sent do
        lead <- getLead
        revealLeadsUntilWitness lead
        doStep 1 msg
      shuffleIntoLeadsDeck . only =<< fetchCard Enemies.crookedCops
      advanceActDeck attrs
      pure a
    {- The Witness revealed from the Leads deck is only in play once its
    revelation has resolved, so the Abductor is placed a step later. -}
    DoStep 1 (AdvanceAct aid _ _) | aid == attrs.id -> do
      void $ sendAbductorToAWitness attrs
      pure a
    _ -> TakingOverTheSearch <$> liftRunMessage msg attrs
