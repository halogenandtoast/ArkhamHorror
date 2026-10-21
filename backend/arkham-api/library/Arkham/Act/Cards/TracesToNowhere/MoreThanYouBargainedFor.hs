module Arkham.Act.Cards.TracesToNowhere.MoreThanYouBargainedFor (moreThanYouBargainedFor) where

import Arkham.Ability
import Arkham.Act.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Trait (Trait (Witness))

newtype MoreThanYouBargainedFor = MoreThanYouBargainedFor ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moreThanYouBargainedFor :: ActCard MoreThanYouBargainedFor
moreThanYouBargainedFor = act (2, A) MoreThanYouBargainedFor Cards.moreThanYouBargainedFor Nothing

instance HasAbilities MoreThanYouBargainedFor where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (notExists (AssetWithTrait Witness) <> not_ (ScenarioDeckWithCard LeadsDeck))
        $ forced
        $ RoundEnds #when
    , restricted a 2 (ChaosTokenCountIs (SealedOnScenarioReference AnyChaosToken) (atLeast 2))
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage MoreThanYouBargainedFor where
  runMessage msg a@(MoreThanYouBargainedFor attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator $ investigatorDefeated attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct aid _ _ | aid == attrs.id && onSide B attrs -> do
      sealed <- getSealedOnScenarioReference
      let isSealed f = f `elem` sealed
      when (isSealed #skull && isSealed #cultist)
        $ shuffleIntoLeadsDeck
        =<< traverse
          fetchCard
          [Assets.linaHusserlTheNextBigThing, Assets.vicarYangTryingToDoTheRightThing]
      when (isSealed #tablet && isSealed #cultist)
        $ shuffleIntoLeadsDeck
        =<< traverse
          fetchCard
          [Assets.albrechtHusserlRecklessGambler, Assets.matildeCorreaInvestigativeReporter]
      when (isSealed #skull && isSealed #tablet)
        $ shuffleIntoLeadsDeck
        . only
        =<< fetchCard Assets.eliasHusserlJrFleeingTheCity

      whenM (remembered TheInvestigatorsFoundSomethingStrangeInTheFuneralHomeBasement) do
        selectForMaybeM (locationIs Locations.duterteFuneralHomeAfterHours) \lid ->
          swapLocation lid =<< genCard Locations.duterteFuneralHomeSomethingInTheBasement
      advanceActDeck attrs
      pure a
    _ -> MoreThanYouBargainedFor <$> liftRunMessage msg attrs
