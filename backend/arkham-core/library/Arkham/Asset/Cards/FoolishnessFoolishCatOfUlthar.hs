module Arkham.Asset.Cards.FoolishnessFoolishCatOfUlthar (
  foolishnessFoolishCatOfUlthar,
  FoolishnessFoolishCatOfUlthar (..),
)
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card.CardCode
import Arkham.Id
import Arkham.Modifier
import Arkham.Projection
import Arkham.SkillType (allSkills)
import Arkham.Token
import Arkham.Window (mkAfter)
import Arkham.Window qualified as Window
import Data.UUID qualified as UUID

newtype FoolishnessFoolishCatOfUlthar = FoolishnessFoolishCatOfUlthar AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foolishnessFoolishCatOfUlthar :: AssetCard FoolishnessFoolishCatOfUlthar
foolishnessFoolishCatOfUlthar =
  allyWith FoolishnessFoolishCatOfUlthar Cards.foolishnessFoolishCatOfUlthar (1, 4)
    $ (tokensL %~ setTokens Horror 3)

instance HasModifiersFor FoolishnessFoolishCatOfUlthar where
  getModifiersFor (InvestigatorTarget iid) (FoolishnessFoolishCatOfUlthar a) | a `controlledBy` iid = do
    horror <- field AssetHorror a.id
    pure $ toModifiers a $ guard (horror == 0) *> [SkillModifier stype 1 | stype <- allSkills]
  getModifiersFor target (FoolishnessFoolishCatOfUlthar a)
    | isTarget a target =
        pure $ toModifiers a [HealHorrorOnThisAsIfInvestigator "05001"] -- see Rational Though
  getModifiersFor _ _ = pure []

instance RunMessage FoolishnessFoolishCatOfUlthar where
  runMessage msg (FoolishnessFoolishCatOfUlthar attrs) = runQueueT $ case msg of
    HealHorror (InvestigatorTarget iid) source amount | unCardCode (unInvestigatorId iid) == UUID.toText (unAssetId $ toId attrs) -> do
      checkWindows [mkAfter $ Window.Healed #horror (toTarget attrs) source amount]
      pure
        . FoolishnessFoolishCatOfUlthar
        $ attrs
        & (tokensL %~ subtractTokens Horror amount)
    HealHorrorDirectly (InvestigatorTarget iid) _ amount | unCardCode (unInvestigatorId iid) == UUID.toText (unAssetId $ toId attrs) -> do
      -- USE ONLY WHEN NO CALLBACKS
      pure
        . FoolishnessFoolishCatOfUlthar
        $ attrs
        & (tokensL %~ subtractTokens Horror amount)
    _ -> FoolishnessFoolishCatOfUlthar <$> lift (runMessage msg attrs)
