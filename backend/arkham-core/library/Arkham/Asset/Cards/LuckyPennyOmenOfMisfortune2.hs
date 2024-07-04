module Arkham.Asset.Cards.LuckyPennyOmenOfMisfortune2 (
  luckyPennyOmenOfMisfortune2,
  LuckyPennyOmenOfMisfortune2 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.ChaosToken
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher
import Arkham.Modifier

newtype LuckyPennyOmenOfMisfortune2 = LuckyPennyOmenOfMisfortune2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyPennyOmenOfMisfortune2 :: AssetCard LuckyPennyOmenOfMisfortune2
luckyPennyOmenOfMisfortune2 = asset LuckyPennyOmenOfMisfortune2 Cards.luckyPennyOmenOfMisfortune2

instance HasAbilities LuckyPennyOmenOfMisfortune2 where
  getAbilities (LuckyPennyOmenOfMisfortune2 x) =
    [ restrictedAbility x 1 (DuringSkillTest $ YourSkillTest #any)
        $ forced (RevealChaosToken #when You $ oneOf [#bless, #curse])
    ]

data Coin = Heads | Tails

instance RunMessage LuckyPennyOmenOfMisfortune2 where
  runMessage msg a@(LuckyPennyOmenOfMisfortune2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      coin <- sample $ Heads :| [Tails]
      case coin of
        Heads -> do
          send $ "The coin lands on heads. Treating token as " <> format @ChaosTokenFace #bless
          skillTestModifier attrs token (ChaosTokenFaceModifier [#bless])
        Tails -> do
          send $ "The coin lands on tails. Treating token as " <> format @ChaosTokenFace #curse
          skillTestModifier attrs token (ChaosTokenFaceModifier [#curse])
          when (token.face == #bless) do
            drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> LuckyPennyOmenOfMisfortune2 <$> liftRunMessage msg attrs
