module Arkham.Homebrew.CircusExMortis.Assets.IllusoryLocus (illusoryLocus) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Cost
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Matcher

newtype IllusoryLocus = IllusoryLocus AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illusoryLocus :: AssetCard IllusoryLocus
illusoryLocus = asset IllusoryLocus Cards.illusoryLocus

instance HasAbilities IllusoryLocus where
  getAbilities (IllusoryLocus x) = [restricted x 1 OnSameLocation actionAbility]

instance RunMessage IllusoryLocus where
  runMessage msg a@(IllusoryLocus attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      totalClues <- getSpendableClueCountOf $ at_ $ locationWithAsset attrs
      if totalClues > 0
        then chooseAmountI18n iid "clues" "$clues" 0 totalClues attrs
        else requestChaosTokens_ iid (attrs.ability 1) 4
      pure $ a & setMeta @Int 0
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) -> do
      spendCluesAsAGroupMatch n $ at_ $ locationWithAsset attrs
      requestChaosTokens_ iid (attrs.ability 1) (4 + 4 * n)
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) (map (.face) -> faces) -> do
      let moons = toResultDefault 0 attrs.meta + count (== MoonToken) faces
      -- revealed [bless]/[curse] are ignored and replaced; they stay set aside so
      -- their replacements come from the rest of the bag
      case count (`elem` [#bless, #curse]) faces of
        0 -> do
          continue_ iid
          placeTokens (attrs.ability 1) attrs #clue moons
          resetChaosTokens (attrs.ability 1)
        ignored -> requestChaosTokens_ iid (attrs.ability 1) ignored
      pure $ a & setMeta moons
    _ -> IllusoryLocus <$> liftRunMessage msg attrs
