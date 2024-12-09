module Arkham.Investigator.Cards.WendyAdams (wendyAdams) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Window qualified as Window

newtype WendyAdams = WendyAdams InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

wendyAdams :: InvestigatorCard WendyAdams
wendyAdams =
  investigator WendyAdams Cards.wendyAdams
    $ Stats {health = 7, sanity = 7, willpower = 4, intellect = 3, combat = 1, agility = 4}

instance HasChaosTokenValue WendyAdams where
  getChaosTokenValue iid ElderSign (WendyAdams attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 0
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasAbilities WendyAdams where
  getAbilities (WendyAdams attrs) =
    [ playerLimit PerTestOrAbility
        $ restricted attrs 1 Self
        $ ReactionAbility (Matcher.RevealChaosToken #when You AnyChaosToken)
        $ HandDiscardCost 1 #any
    ]

instance RunMessage WendyAdams where
  runMessage msg i@(WendyAdams attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> [token]) _ -> do
      cancelChaosToken (attrs.ability 1) token
      pushAll [ReturnChaosTokens [token], UnfocusChaosTokens]
      drawAnotherChaosToken iid
      pure i
    ElderSignEffect (is attrs -> True) -> do
      whenAny (assetIs Assets.wendysAmulet) passSkillTest
      pure i
    _ -> WendyAdams <$> liftRunMessage msg attrs
