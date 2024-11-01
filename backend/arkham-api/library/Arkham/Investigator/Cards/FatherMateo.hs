module Arkham.Investigator.Cards.FatherMateo (fatherMateo, FatherMateo (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers.Window (getRevealedChaosTokens)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype FatherMateo = FatherMateo InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

fatherMateo :: InvestigatorCard FatherMateo
fatherMateo =
  investigator FatherMateo Cards.fatherMateo
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 3, combat = 2, agility = 3}

instance HasAbilities FatherMateo where
  getAbilities (FatherMateo a) =
    [ playerLimit PerGame
        $ restrictedAbility a 1 Self
        $ freeReaction (Matcher.RevealChaosToken #when (affectsOthers Anyone) #autofail)
    ]

instance HasChaosTokenValue FatherMateo where
  getChaosTokenValue iid ElderSign (FatherMateo attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign NoModifier
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage FatherMateo where
  runMessage msg i@(FatherMateo attrs) = runQueueT $ case msg of
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      afterSkillTest $ push $ Do msg
      passSkillTest
      pure i
    Do (ResolveChaosToken _ ElderSign iid) | attrs `is` iid -> do
      isTurn <- iid <=~> TurnInvestigator
      canDraw <- can.draw.cards iid
      canGainResources <- can.gain.resources iid
      chooseOrRunOneM iid do
        when (canDraw || canGainResources) $ do
          labeled "Draw 1 card and gain 1 resource" do
            drawCardsIfCan iid ElderSign 1
            gainResourcesIfCan iid ElderSign 1
        when isTurn $ do
          labeled "Take an additional action this turn" $ gainActions iid ElderSign 1
      pure i
    UseCardAbility _ (isSource attrs -> True) 1 (getRevealedChaosTokens -> [token]) _ -> do
      chaosTokenEffect (attrs.ability 1) token (ChaosTokenFaceModifier [ElderSign])
      pure i
    _ -> FatherMateo <$> liftRunMessage msg attrs
