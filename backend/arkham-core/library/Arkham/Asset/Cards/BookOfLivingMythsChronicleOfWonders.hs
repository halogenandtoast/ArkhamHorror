module Arkham.Asset.Cards.BookOfLivingMythsChronicleOfWonders (
  bookOfLivingMythsChronicleOfWonders,
  BookOfLivingMythsChronicleOfWonders (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype BookOfLivingMythsChronicleOfWonders = BookOfLivingMythsChronicleOfWonders AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfLivingMythsChronicleOfWonders :: AssetCard BookOfLivingMythsChronicleOfWonders
bookOfLivingMythsChronicleOfWonders = asset BookOfLivingMythsChronicleOfWonders Cards.bookOfLivingMythsChronicleOfWonders

instance HasAbilities BookOfLivingMythsChronicleOfWonders where
  getAbilities (BookOfLivingMythsChronicleOfWonders a) =
    [ controlledAbility
        a
        1
        (oneOf [ChaosTokenCountIs #bless (atLeast 1), ChaosTokenCountIs #curse (atLeast 1)])
        $ ReactionAbility
          (WouldRevealChaosToken #when $ affectsOthers $ InvestigatorAt YourLocation)
          (exhaust a)
    ]

instance RunMessage BookOfLivingMythsChronicleOfWonders where
  runMessage msg a@(BookOfLivingMythsChronicleOfWonders attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      tokens <- getOnlyChaosTokensInBag
      let bIn = filter ((== #bless) . (.face)) tokens
      let cIn = filter ((== #curse) . (.face)) tokens

      chooseOrRunOne iid
        $ [Label "Resolve {bless} token" [DoStep 1 msg] | length bIn >= length cIn]
        <> [Label "Resolve {curse} token" [DoStep 2 msg] | length cIn >= length bIn]
      pure a
    DoStep n (UseThisAbility iid (isSource attrs -> True) 1) -> do
      tokens <- getOnlyChaosTokensInBag
      let bIn = filter ((== #bless) . (.face)) tokens
      let cIn = filter ((== #curse) . (.face)) tokens
      case take 1 (if n == 1 then bIn else cIn) of
        [token] -> do
          cancelTokenDraw
          push $ SetChaosTokenAside token
          checkWhen $ Window.RevealChaosToken iid token
          withSkillTest \sid ->
            push $ RequestedChaosTokens (SkillTestSource sid) (Just iid) [token]
        _ -> error "invalid token"
      pure a
    _ -> BookOfLivingMythsChronicleOfWonders <$> liftRunMessage msg attrs
