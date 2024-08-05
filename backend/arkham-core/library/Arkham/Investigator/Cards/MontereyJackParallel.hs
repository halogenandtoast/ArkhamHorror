module Arkham.Investigator.Cards.MontereyJackParallel (
  montereyJackParallel,
  MontereyJackParallel (..),
)
where

import Arkham.Ability
import Arkham.Capability
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Modifiers (getAdditionalSearchTargets)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Strategy

newtype MontereyJackParallel = MontereyJackParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

montereyJackParallel :: InvestigatorCard MontereyJackParallel
montereyJackParallel =
  investigator MontereyJackParallel Cards.montereyJackParallel
    $ Stats {health = 8, sanity = 6, willpower = 1, intellect = 4, combat = 2, agility = 5}

instance HasAbilities MontereyJackParallel where
  getAbilities (MontereyJackParallel a) =
    [ playerLimit PerRound
        $ restrictedAbility a 1 (Self <> can.search.deck You)
        $ freeReaction
        $ DiscoveringLastClue #after You (YourLocation <> LocationWithShroud (atLeast 1))
    ]

instance HasChaosTokenValue MontereyJackParallel where
  getChaosTokenValue iid ElderSign (MontereyJackParallel attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MontereyJackParallel where
  runMessage msg i@(MontereyJackParallel attrs) = runQueueT $ case msg of
    ElderSignEffect (is attrs -> True) -> do
      n <- selectCount $ assetControlledBy attrs.id <> oneOf [#charm, #relic]
      gainResourcesIfCan attrs.id (#elderSign :: Source) n
      pure i
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        field LocationShroud lid >>= traverse_ \x -> do
          search
            iid
            (attrs.ability 1)
            iid
            [fromTopOfDeck x]
            (PlayableCardWithCostReduction NoAction x $ basic $ #asset <> oneOf [#charm, #relic])
            (defer attrs IsNotDraw)
      pure i
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      additionalTargets <- getAdditionalSearchTargets iid
      chooseUpToN
        iid
        (1 + additionalTargets)
        "Do not play"
        [targetLabel card [handleTargetChoice iid (attrs.ability 1) card] | card <- cards]
      pure i
    SearchFound iid (isTarget attrs -> True) _ [] -> do
      chooseOne iid [Label "No Card Founds" []]
      pure i
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      withLocationOf iid \lid -> do
        field LocationShroud lid >>= traverse_ \x -> do
          push $ AddFocusedToHand iid (toTarget iid) FromDeck cid
          costModifier (attrs.ability 1) cid (ReduceCostOf (CardWithId cid) x)
          putCardIntoPlay iid =<< getCard cid
      pure i
    _ -> MontereyJackParallel <$> liftRunMessage msg attrs
