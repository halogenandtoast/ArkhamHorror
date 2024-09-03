module Arkham.Agenda.Cards.TheNightHowls (
  TheNightHowls (..),
  theNightHowls,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Campaign
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Curse, Omen, Witch))

newtype TheNightHowls = TheNightHowls AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNightHowls :: AgendaCard TheNightHowls
theNightHowls = agenda (2, A) TheNightHowls Cards.theNightHowls (Static 12)

instance HasAbilities TheNightHowls where
  getAbilities (TheNightHowls a) =
    [ restrictedAbility a 1 (EnemyCriteria $ EnemyExists $ EnemyWithTrait Witch)
        $ ForcedAbility
        $ RoundEnds Timing.AtIf
    ]

instance RunMessage TheNightHowls where
  runMessage msg a@(TheNightHowls attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- getInvestigatorIds
      isStandalone <- getIsStandalone
      msgs <-
        if isStandalone
          then pure [SufferTrauma iid 0 1 | iid <- iids]
          else do
            alreadyIncludedMap <-
              matchingCardsAlreadyInDeck
                $ CardWithOneOf
                $ map
                  CardWithTrait
                  [Omen, Curse]
            pure
              $ [ SearchCollectionForRandom iid (toSource attrs)
                  $ BasicWeaknessCard
                  <> CardWithOneOf (map CardWithTrait [Omen, Curse])
                  <> excludeMatcher
                | iid <- iids
                , let cardCodes = findWithDefault mempty iid alreadyIncludedMap
                , let
                    excludeMatcher =
                      if null cardCodes
                        then AnyCard
                        else
                          NotCard
                            ( CardWithOneOf
                                $ map CardWithCardCode
                                $ setToList
                                  cardCodes
                            )
                ]
      pushAll
        $ msgs
        <> [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
      pure a
    RequestedPlayerCard iid (isSource attrs -> True) mcard _ -> do
      case mcard of
        Just card -> push $ AddCardToDeckForCampaign iid card
        Nothing -> push $ SufferTrauma iid 0 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      witchCount <- selectCount $ EnemyWithTrait Witch
      push $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) witchCount
      pure a
    _ -> TheNightHowls <$> runMessage msg attrs
