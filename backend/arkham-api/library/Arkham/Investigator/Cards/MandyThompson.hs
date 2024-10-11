module Arkham.Investigator.Cards.MandyThompson (mandyThompson, MandyThompson (..)) where

import Arkham.Ability
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Strategy
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype MandyThompson = MandyThompson InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

mandyThompson :: InvestigatorCard MandyThompson
mandyThompson =
  investigator MandyThompson Cards.mandyThompson
    $ Stats {health = 6, sanity = 8, willpower = 3, intellect = 5, combat = 1, agility = 3}

-- N.B. ThatInvestigator so far is only used here because we need to not allow
-- it to be any investigator's deck, this NEEDS to be replaced at criteria
-- match
instance HasAbilities MandyThompson where
  getAbilities (MandyThompson attrs) =
    [ playerLimit PerRound
        $ restrictedAbility attrs 1 Self
        $ freeReaction
        $ WouldSearchDeck #when (affectsOthers $ InvestigatorAt YourLocation)
        $ DeckOneOf [EncounterDeck, DeckOf ThatInvestigator]
    ]

instance HasChaosTokenValue MandyThompson where
  getChaosTokenValue iid ElderSign (MandyThompson attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 0)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

getInvestigator :: [Window] -> InvestigatorId
getInvestigator = \case
  ((windowType -> Window.WouldSearchDeck iid _) : _) -> iid
  (_ : rest) -> getInvestigator rest
  _ -> error "Expected investigator"

instance RunMessage MandyThompson where
  runMessage msg i@(MandyThompson attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getInvestigator -> iid') _ -> do
      let source = toAbilitySource attrs 1
      chooseOneM iid do
        labeled "Search 3 additional cards" $ searchModifier source iid' $ SearchDepth 3
        labeled "Resolve 1 additional target of the search" do
          searchModifier source iid' $ AdditionalTargets 1
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      search iid ElderSign iid [fromTopOfDeck 3] #any (DrawOrCommitFound iid 1)
      shuffleDeck iid
      pure i
    _ -> MandyThompson <$> liftRunMessage msg attrs
