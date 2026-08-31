module Arkham.Homebrew.CircusExMortis.Assets.IllusoryLocus (illusoryLocus) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBag.RevealStrategy
import Arkham.Helpers.Cost
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.I18n
import Arkham.Matcher
import Arkham.RequestedChaosTokenStrategy

newtype IllusoryLocus = IllusoryLocus AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illusoryLocus :: AssetCard IllusoryLocus
illusoryLocus = asset IllusoryLocus Cards.illusoryLocus

instance HasAbilities IllusoryLocus where
  getAbilities (IllusoryLocus x) = [restricted x 1 OnSameLocation actionAbility]

revealTokens :: ReverseQueue m => AssetAttrs -> InvestigatorId -> Int -> m ()
revealTokens attrs iid n =
  push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal n) SetAside

instance RunMessage IllusoryLocus where
  runMessage msg a@(IllusoryLocus attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      totalClues <- getSpendableClueCountOf $ InvestigatorAt (locationWithAsset attrs)
      if totalClues > 0
        then withI18n $ chooseAmount' iid "clues" "$clues" 0 totalClues attrs
        else revealTokens attrs iid 4
      pure $ a & overAttrs (setMeta (0 :: Int))
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) -> do
      when (n > 0) do
        iids <- select $ InvestigatorAt (locationWithAsset attrs)
        push $ SpendClues n iids
      revealTokens attrs iid (4 + 4 * n)
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      let faces = map (.face) tokens
      let moons = count (== MoonToken) faces
      let total = toResultDefault 0 attrs.meta + moons
      -- revealed [bless]/[curse] are ignored and replaced; they stay set aside so
      -- their replacements come from the rest of the bag
      case count (`elem` [#bless, #curse]) faces of
        0 -> do
          continue_ iid
          placeTokens (attrs.ability 1) attrs #clue total
          resetChaosTokens (attrs.ability 1)
        ignored -> revealTokens attrs iid ignored
      pure $ a & overAttrs (setMeta total)
    _ -> IllusoryLocus <$> liftRunMessage msg attrs
