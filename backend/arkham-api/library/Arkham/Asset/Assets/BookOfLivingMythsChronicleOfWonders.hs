module Arkham.Asset.Assets.BookOfLivingMythsChronicleOfWonders (
  bookOfLivingMythsChronicleOfWonders,
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window qualified as Window

newtype BookOfLivingMythsChronicleOfWonders = BookOfLivingMythsChronicleOfWonders AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bookOfLivingMythsChronicleOfWonders :: AssetCard BookOfLivingMythsChronicleOfWonders
bookOfLivingMythsChronicleOfWonders = asset BookOfLivingMythsChronicleOfWonders Cards.bookOfLivingMythsChronicleOfWonders

instance HasAbilities BookOfLivingMythsChronicleOfWonders where
  getAbilities (BookOfLivingMythsChronicleOfWonders a) =
    [ controlled a 1 (mapOneOf ((`ChaosTokenCountIs` atLeast 1) . OnlyInBag) [#bless, #curse])
        $ triggered (WouldRevealChaosToken #when $ affectsOthers $ at_ YourLocation) (exhaust a)
    ]

instance RunMessage BookOfLivingMythsChronicleOfWonders where
  runMessage msg a@(BookOfLivingMythsChronicleOfWonders attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      tokens <- getOnlyChaosTokensInBag
      let bIn = filter ((== #bless) . (.face)) tokens
      let cIn = filter ((== #curse) . (.face)) tokens

      chooseOrRunOneM iid do
        when (length bIn >= length cIn) $ labeled "Resolve {bless} token" $ doStep 1 msg
        when (length cIn >= length bIn) $ labeled "Resolve {curse} token" $ doStep 2 msg
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
          withSkillTest \sid -> push $ RequestedChaosTokens (SkillTestSource sid) (Just iid) [token]
        _ -> error "invalid token"
      pure a
    _ -> BookOfLivingMythsChronicleOfWonders <$> liftRunMessage msg attrs
