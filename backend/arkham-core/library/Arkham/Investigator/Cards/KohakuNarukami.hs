module Arkham.Investigator.Cards.KohakuNarukami (
  kohakuNarukami,
  KohakuNarukami (..),
)
where

import Arkham.Ability
import Arkham.Helpers.ChaosBag
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher

newtype KohakuNarukami = KohakuNarukami InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

kohakuNarukami :: InvestigatorCard KohakuNarukami
kohakuNarukami =
  investigator KohakuNarukami Cards.kohakuNarukami
    $ Stats {health = 6, sanity = 8, willpower = 4, intellect = 4, combat = 3, agility = 1}

instance HasAbilities KohakuNarukami where
  getAbilities (KohakuNarukami a) = [restrictedAbility a 1 Self $ freeReaction $ TurnBegins #when You]

instance HasChaosTokenValue KohakuNarukami where
  getChaosTokenValue iid ElderSign (KohakuNarukami attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage KohakuNarukami where
  runMessage msg i@(KohakuNarukami attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      tokens <- getOnlyChaosTokensInBag
      let bIn = filter ((== #bless) . (.face)) tokens
      let cIn = filter ((== #curse) . (.face)) tokens
      bOut <- getRemainingBlessTokens
      cOut <- getRemainingCurseTokens

      chooseOrRunOne iid
        $ [Label "Add 1 {bless} token" [AddChaosToken #bless] | bOut > 0 && length bIn <= length cIn]
        <> [Label "Add 1 {curse} token" [AddChaosToken #curse] | cOut > 0 && length cIn <= length bIn]
        <> [ Label
            "Remove 2 {bless} tokens and 2 {curse} tokens from the chaos bag to take an additional action this turn"
            [ReturnChaosTokens (take 2 bIn <> take 2 cIn), GainActions iid (toSource attrs) 1]
           | length bIn >= 2 && length cIn >= 2
           ]
      pure i
    ElderSignEffect iid | iid == attrs.id -> do
      b <- getRemainingBlessTokens
      c <- getRemainingCurseTokens
      pushWhen (b > 0) $ AddChaosToken #bless
      pushWhen (c > 0) $ AddChaosToken #curse
      pure i
    _ -> KohakuNarukami <$> liftRunMessage msg attrs
