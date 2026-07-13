module Arkham.Homebrew.CircusExMortis.Assets.IllusoryLocus (illusoryLocus) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.RequestedChaosTokenStrategy

newtype IllusoryLocus = IllusoryLocus AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illusoryLocus :: AssetCard IllusoryLocus
illusoryLocus = asset IllusoryLocus Cards.illusoryLocus

instance HasAbilities IllusoryLocus where
  getAbilities (IllusoryLocus x) =
    [restricted x 1 OnSameLocation actionAbility]

instance RunMessage IllusoryLocus where
  runMessage msg a@(IllusoryLocus attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Investigators at this location may spend any amount of clues as a group
      -- for 4 additional reveals per clue spent.
      iids <- select $ InvestigatorAt (locationWithAsset attrs)
      totalClues <- sum <$> for iids (field InvestigatorClues)
      if totalClues > 0
        then chooseAmount iid "Clues to spend" "Clues" 0 totalClues attrs
        else push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal 4) SetAside
      pure a
    ResolveAmounts iid (getChoiceAmount "Clues" -> n) (isTarget attrs -> True) -> do
      when (n > 0) do
        iids <- select $ InvestigatorAt (locationWithAsset attrs)
        push $ SpendClues n iids
      push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal (4 + 4 * n)) SetAside
      pure a
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just _iid) tokens -> do
      -- TODO(homebrew): bless/curse tokens should be ignored and re-revealed;
      -- this campaign adds none to the bag, so we count them as blanks for now.
      let moons = count ((== MoonToken) . (.face)) tokens
      when (moons > 0) $ placeTokens (attrs.ability 1) attrs #clue moons
      push $ ResetChaosTokens (attrs.ability 1)
      pure a
    _ -> IllusoryLocus <$> liftRunMessage msg attrs
