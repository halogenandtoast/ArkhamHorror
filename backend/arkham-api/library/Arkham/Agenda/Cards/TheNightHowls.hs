module Arkham.Agenda.Cards.TheNightHowls (TheNightHowls (..), theNightHowls) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Campaign
import Arkham.Helpers.Scenario (getIsStandalone)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Trait (Trait (Curse, Omen, Witch))

newtype TheNightHowls = TheNightHowls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNightHowls :: AgendaCard TheNightHowls
theNightHowls = agenda (2, A) TheNightHowls Cards.theNightHowls (Static 12)

instance HasAbilities TheNightHowls where
  getAbilities (TheNightHowls a) =
    [ restrictedAbility a 1 (exists $ EnemyWithTrait Witch <> at_ (locationIs Locations.witchesCircle))
        $ forced
        $ RoundEnds #at
    ]

instance RunMessage TheNightHowls where
  runMessage msg a@(TheNightHowls attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      isStandalone <- getIsStandalone
      if isStandalone
        then eachInvestigator (`sufferMentalTrauma` 1)
        else do
          alreadyIncludedMap <- matchingCardsAlreadyInDeck $ mapOneOf CardWithTrait [Omen, Curse]
          eachInvestigator $ \iid -> do
            let cardCodes = setToList $ findWithDefault mempty iid alreadyIncludedMap
            let excludeMatcher = if null cardCodes then AnyCard else not_ (mapOneOf CardWithCardCode cardCodes)
            searchCollectionForRandom iid attrs
              $ BasicWeaknessCard
              <> mapOneOf withTrait [Omen, Curse]
              <> excludeMatcher
      eachInvestigator (kill attrs)
      pure a
    RequestedPlayerCard iid (isSource attrs -> True) mcard _ -> do
      case mcard of
        Just card -> push $ AddCardToDeckForCampaign iid card
        Nothing -> sufferMentalTrauma iid 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs
        =<< selectCount (EnemyWithTrait Witch <> at_ (locationIs Locations.witchesCircle))
      pure a
    _ -> TheNightHowls <$> liftRunMessage msg attrs
