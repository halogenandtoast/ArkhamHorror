module Arkham.Homebrew.AgainstTheWendigo.Assets.IthaquasKnowledge (ithaquasKnowledge) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards
import Arkham.Matcher (InvestigatorMatcher (You), WindowMatcher (SuffersMentalTrauma))

newtype IthaquasKnowledge = IthaquasKnowledge AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ithaquasKnowledge :: AssetCard IthaquasKnowledge
ithaquasKnowledge = asset IthaquasKnowledge Cards.ithaquasKnowledge

instance HasAbilities IthaquasKnowledge where
  getAbilities (IthaquasKnowledge a) =
    [ controlled a 1 (DuringTurn You) $ FastAbility Free
    , -- "Forced - If you suffer a mental trauma: Exile Ithaqua's Knowledge."
      controlled_ a 2 $ forced $ SuffersMentalTrauma #after You
    ]

instance RunMessage IthaquasKnowledge where
  runMessage msg a@(IthaquasKnowledge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifier iid (attrs.ability 1) iid (SkillModifier #intellect 3)
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      directHorror iid (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      {- 'exile' only takes the asset out of play. The copy that follows you
      between scenarios is the campaign story card, so that has to go as well or
      it is back in play at the next setup. -}
      exile attrs
      removeCampaignCardFromDeck iid Cards.ithaquasKnowledge
      pure a
    _ -> IthaquasKnowledge <$> liftRunMessage msg attrs
